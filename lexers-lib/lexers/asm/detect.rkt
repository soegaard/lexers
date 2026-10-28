#lang racket/base

;;;
;;; Assembly Selection
;;;

;; Inspectable, intentionally conservative assembly profile selection.  This
;; module records what a caller asked for without enabling uncalibrated content
;; detection or treating compatible source as proof of a toolchain identity.

;; asm-selection?              : any/c -> boolean?
;;   Recognize a resolved assembly profile request.
;; asm-selection-requested     : asm-selection? -> (or/c symbol? #f)
;;   Extract the requested profile name.
;; asm-selection-effective     : asm-selection? -> (or/c symbol? #f)
;;   Extract the implemented profile selected for lexing, if any.
;; asm-selection-origin        : asm-selection? -> symbol?
;;   Extract the source of the selection decision.
;; asm-selection-diagnostics   : asm-selection? -> (listof asm-diagnostic?)
;;   Extract selection diagnostics.
;; asm-evidence?               : any/c -> boolean?
;;   Recognize one bounded selection evidence item.
;; asm-detection?              : any/c -> boolean?
;;   Recognize a versioned assembly detection record.
;; asm-content-evidence?       : any/c -> boolean?
;;   Recognize one profile-scanner observation used by bounded probing.
;; asm-detection-conflict?     : any/c -> boolean?
;;   Recognize rival evidence hidden by a candidate's comment rule.
;; asm-detection-candidate?    : any/c -> boolean?
;;   Recognize one scored dialect-and-architecture candidate.
;; asm-auto-selection?         : any/c -> boolean?
;;   Recognize the conservative conclusion from a bounded profile probe.
;; asm-probe?                  : any/c -> boolean?
;;   Recognize a versioned bounded automatic-detection report.
;; resolve-asm-selection       : keyword-arguments -> asm-selection?
;;   Resolve an explicit profile and, when needed, its target architecture.
;; detect-asm-bytes            : bytes? keyword-arguments -> asm-detection?
;;   Inspect BOM evidence only; automatic dialect selection remains disabled.
;; probe-asm-bytes             : bytes? keyword-arguments -> asm-probe?
;;   Score profile-scanner evidence without enabling uncalibrated selection.

(provide (struct-out asm-selection)
         (struct-out asm-evidence)
         (struct-out asm-detection)
         (struct-out asm-content-evidence)
         (struct-out asm-detection-conflict)
         (struct-out asm-detection-candidate)
         (struct-out asm-auto-selection)
         (struct-out asm-probe)
         resolve-asm-selection
         detect-asm-bytes
         probe-asm-bytes)

(require racket/list
         (only-in "../asm.rkt"
                  asm-diagnostic
                  asm-diagnostic-code
                  asm-derived-token-byte-span
                  asm-derived-token-category
                  asm-derived-token-has-tag?
                  asm-derived-token-text
                  asm-profile-architecture
                  asm-profile-status
                  asm-profiles
                  make-asm-derived-lexer)
         (only-in "../private/source.rkt"
                  source-span
                  source-span-end
                  source-span-start)
         (only-in "../private/asm/profiles.rkt"
                  asm-profile-architectures))

;; A requested and effective profile remain separate when a profile is absent.
(struct asm-selection (requested effective origin diagnostics) #:transparent)

;; One bounded, source-location-aware piece of inspection evidence.
(struct asm-evidence (dimension value origin confidence byte-span) #:transparent)

;; A stable, versioned detection result without renderer-specific information.
(struct asm-detection (schema-version selection evidence diagnostics encoding
                                       inspected-byte-count truncated?)
  #:transparent)

;; One candidate-scanner observation with its lexical rule, dimensions, and
;; bounded source context. Contributions are heuristic weights, not probabilities.
(struct asm-content-evidence (rule-id feature-class dimensions contribution
                                          byte-span context)
  #:transparent)

;; A strong rival observation that a candidate classified as comment text.
(struct asm-detection-conflict (rule-id rival-dialect byte-span context)
  #:transparent)

;; A separately scored dialect-and-architecture tuple from one bounded probe.
(struct asm-detection-candidate (dialect architecture dialect-score architecture-score
                                         score feature-classes evidence conflicts)
  #:transparent)

;; A conservative bounded-probe conclusion. `automatic-enabled?` remains false
;; until held-out calibration has qualified the candidate profile.
(struct asm-auto-selection (band dialect architecture compatible-candidates score
                                 margin automatic-enabled? reason)
  #:transparent)

;; A versioned source-prefix probe with inspectable candidate evidence.
(struct asm-probe (schema-version selection candidates evidence diagnostics encoding
                                   inspected-byte-count truncated?)
  #:transparent)

;; profile-diagnostic : symbol? symbol? string? -> asm-diagnostic?
;;   Create an unspanned selection diagnostic.
(define (profile-diagnostic code severity message)
  (asm-diagnostic code severity message #f))

;; resolve-asm-selection : keyword-arguments -> asm-selection?
;;   Resolve an explicit profile without guessing a dialect or GNU target.
(define (resolve-asm-selection #:dialect      [dialect #f]
                               #:architecture [architecture #f])
  (unless (or (not architecture)
              (symbol? architecture))
    (raise-argument-error 'resolve-asm-selection "(or/c symbol? #f)" architecture))
  (cond
    [(not dialect)
     (asm-selection #f
                    #f
                    'unknown
                    (list (profile-diagnostic 'automatic-selection-disabled
                                              'info
                                              "Automatic assembly dialect selection is not enabled.")))]
    [(not (symbol? dialect))
     (raise-argument-error 'resolve-asm-selection "(or/c symbol? #f)" dialect)]
    [else
     (define profile
       (hash-ref asm-profiles dialect #f))
     (cond
       [(not profile)
        (asm-selection dialect
                       #f
                       'explicit
                       (list (profile-diagnostic 'unknown-profile
                                                 'error
                                                 "The requested assembly profile is unknown.")))]
       [(not (eq? (asm-profile-status profile) 'implemented))
        (asm-selection dialect
                       #f
                       'explicit
                       (list (profile-diagnostic 'profile-unavailable
                                                 'warning
                                                 "The requested assembly profile is recognized but unavailable.")))]
       [(pair? (asm-profile-architectures dialect))
        (define supported
          (asm-profile-architectures dialect))
        (cond
          [(not architecture)
           (asm-selection dialect
                          #f
                          'explicit
                          (list (profile-diagnostic 'architecture-required
                                                    'error
                                                    "The requested assembly profile requires an explicit architecture.")))]
          [(member architecture supported)
           (asm-selection dialect dialect 'explicit '())]
          [else
           (asm-selection dialect
                          #f
                          'explicit
                          (list (profile-diagnostic 'unsupported-architecture
                                                    'error
                                                    "The selected architecture is not supported by this assembly profile.")))])]
       [(and architecture
             (not (eq? architecture (asm-profile-architecture profile))))
        (asm-selection dialect
                       #f
                       'explicit
                       (list (profile-diagnostic 'unsupported-architecture
                                                 'error
                                                 "The selected architecture is not supported by this assembly profile.")))]
       [(eq? (asm-profile-status profile) 'implemented)
        (asm-selection dialect dialect 'explicit '())])]))

;; bom-evidence : bytes? -> (values symbol? (listof asm-evidence))
;;   Report only encoding facts whose byte-order marks are complete and exact.
(define (bom-evidence sample)
  (cond
    [(and (>= (bytes-length sample) 3)
          (equal? (subbytes sample 0 3) #"\357\273\277"))
     (values 'utf-8
             (list (asm-evidence 'encoding
                                 'utf-8
                                 'content
                                 'certain
                                 (source-span 0 3))))]
    [(and (>= (bytes-length sample) 2)
          (equal? (subbytes sample 0 2) #"\377\376"))
     (values 'utf-16le
             (list (asm-evidence 'encoding
                                 'utf-16le
                                 'content
                                 'certain
                                 (source-span 0 2))))]
    [(and (>= (bytes-length sample) 2)
          (equal? (subbytes sample 0 2) #"\376\377"))
     (values 'utf-16be
             (list (asm-evidence 'encoding
                                 'utf-16be
                                 'content
                                 'certain
                                 (source-span 0 2))))]
    [else
     (values 'unknown '())]))

;; detect-asm-bytes : bytes? keyword-arguments -> asm-detection?
;;   Report bounded representation facts without enabling dialect scoring.
(define (detect-asm-bytes input
                          #:dialect      [dialect #f]
                          #:architecture [architecture #f]
                          #:limit        [limit 32768])
  (unless (bytes? input)
    (raise-argument-error 'detect-asm-bytes "bytes?" input))
  (unless (exact-positive-integer? limit)
    (raise-argument-error 'detect-asm-bytes "exact-positive-integer?" limit))
  (define inspected-byte-count
    (min (bytes-length input) limit))
  (define sample
    (subbytes input 0 inspected-byte-count))
  (define-values (encoding encoding-evidence)
    (bom-evidence sample))
  (define selection
    (resolve-asm-selection #:dialect      dialect
                           #:architecture architecture))
  (define truncation-diagnostic
    (cond
      [(< inspected-byte-count (bytes-length input))
       (list (profile-diagnostic 'inspection-truncated
                                 'info
                                 "Assembly inspection stopped at its configured byte limit."))]
      [else
       '()]))
  (asm-detection 1
                 selection
                 encoding-evidence
                 (append (asm-selection-diagnostics selection)
                         truncation-diagnostic)
                 encoding
                 inspected-byte-count
                 (< inspected-byte-count (bytes-length input))))

;; candidate-run : symbol? symbol? (listof asm-content-evidence?) (listof source-span?) -> candidate-run?
;;   Retain one candidate scanner's evidence and comment spans during one probe.
(struct candidate-run (dialect architecture evidence comment-spans) #:transparent)

;; Calibration gate. No profile appears here until held-out evaluation has
;; established its calibration; inspectable probes remain available first.
(define calibrated-automatic-profiles
  (hasheq))

;; probe-tuples : -> (listof (cons/c symbol? symbol?))
;;   Enumerate implemented non-generic profile and target combinations deterministically.
(define (probe-tuples)
  (for/fold ([tuples '()])
            ([name (in-list (sort (hash-keys asm-profiles)
                                  string<?
                                  #:key symbol->string))])
    (define profile
      (hash-ref asm-profiles name))
    (cond
      [(or (not (eq? (asm-profile-status profile) 'implemented))
           (eq? name 'generic))
       tuples]
      [else
       (define architectures
         (asm-profile-architectures name))
       (define supported-architectures
         (cond
           [(pair? architectures)
            architectures]
           [else
            (list (asm-profile-architecture profile))]))
       (append tuples
               (for/list ([architecture (in-list supported-architectures)])
                 (cons name architecture)))])))

;; token-context : asm-derived-token? -> (listof symbol?)
;;   Record whether one observation occurs in macro or conditional source context.
(define (token-context token)
  (define context
    (append (cond
              [(asm-derived-token-has-tag? token 'asm-macro-body)
               '(macro-body)]
              [else
               '()])
            (cond
              [(asm-derived-token-has-tag? token 'asm-conditional-region)
               '(conditional-region)]
              [else
               '()])))
  (cond
    [(null? context)
     '(unconditional)]
    [else
     context]))

;; unconditional-evidence? : asm-content-evidence? -> boolean?
;;   Determine whether an observation can affect a file-level selection score.
(define (unconditional-evidence? evidence)
  (and (member 'unconditional (asm-content-evidence-context evidence)) #t))

;; make-content-evidence : asm-derived-token? symbol? symbol? (listof symbol?) exact-integer? -> asm-content-evidence?
;;   Attach one stable scoring rule to an already-scanned source fragment.
(define (make-content-evidence token rule-id feature-class dimensions contribution)
  (asm-content-evidence rule-id
                        feature-class
                        dimensions
                        contribution
                        (asm-derived-token-byte-span token)
                        (token-context token)))

;; token-word : asm-derived-token? -> symbol?
;;   Normalize a scanner token only after its profile grammar has identified it.
(define (token-word token)
  (string->symbol (string-downcase (asm-derived-token-text token))))

;; profile-token-evidence : symbol? symbol? asm-derived-token? -> (listof asm-content-evidence?)
;;   Derive profile evidence from shared-scanner tags, never from a parallel grammar.
(define (profile-token-evidence dialect architecture token)
  (define word
    (token-word token))
  (define directive?
    (asm-derived-token-has-tag? token 'asm-directive))
  (define instruction?
    (asm-derived-token-has-tag? token 'asm-instruction))
  (define register?
    (asm-derived-token-has-tag? token 'asm-register))
  (define fallback
    (cond
      [instruction?
       (list (make-content-evidence token
                                    'instruction
                                    'instruction
                                    '(architecture)
                                    20))]
      [register?
       (list (make-content-evidence token
                                    'register
                                    'register
                                    '(architecture)
                                    20))]
      [directive?
       (list (make-content-evidence token
                                    'directive-convention
                                    'convention
                                    '(dialect)
                                    10))]
      [else
       '()]))
  (case dialect
    [(ca65)
     (cond
       [(and directive? (eq? word '.setcpu))
        (list (make-content-evidence token
                                     'ca65-setcpu
                                     'declaration
                                     '(dialect architecture)
                                     60))]
       [(and directive? (member word '(.feature .importzp .exportzp)))
        (list (make-content-evidence token
                                     'ca65-directive-family
                                     'distinctive
                                     '(dialect)
                                     30))]
       [(or (asm-derived-token-has-tag? token 'ca65-cheap-local)
            (asm-derived-token-has-tag? token 'ca65-anonymous-label-reference))
        (list (make-content-evidence token
                                     'ca65-local-label
                                     'distinctive
                                     '(dialect)
                                     30))]
       [else
        fallback])]
    [(acme)
     (cond
       [(and directive? (eq? word '!cpu))
        (list (make-content-evidence token
                                     'acme-cpu
                                     'declaration
                                     '(dialect architecture)
                                     60))]
       [(and directive? (member word '(!zone !source !to !pseudopc)))
        (list (make-content-evidence token
                                     'acme-directive-family
                                     'distinctive
                                     '(dialect)
                                     30))]
       [(asm-derived-token-has-tag? token 'acme-macro-call)
        (list (make-content-evidence token
                                     'acme-macro-call
                                     'distinctive
                                     '(dialect)
                                     30))]
       [else
        fallback])]
    [(kick)
     (cond
       [(and directive? (member word '(|#import| |#importonce|)))
        (list (make-content-evidence token
                                     'kick-import
                                     'declaration
                                     '(dialect architecture)
                                     60))]
       [(or (asm-derived-token-has-tag? token 'kick-script-region)
            (asm-derived-token-has-tag? token 'kick-anonymous-label))
        (list (make-content-evidence token
                                     'kick-script
                                     'distinctive
                                     '(dialect)
                                     30))]
       [else
        fallback])]
    [(|64tass|)
     (cond
       [(and directive? (eq? word '.cpu))
        (list (make-content-evidence token
                                     '64tass-cpu
                                     'declaration
                                     '(dialect architecture)
                                     60))]
       [(or (asm-derived-token-has-tag? token '|64tass-namespace-region|)
            (asm-derived-token-has-tag? token '|64tass-emitted-encoding|))
        (list (make-content-evidence token
                                     '64tass-namespace-or-encoding
                                     'distinctive
                                     '(dialect)
                                     30))]
       [else
        fallback])]
    [(gas)
     (case architecture
       [(x86)
        (cond
          [(and directive? (member word '(.intel_syntax .att_syntax)))
           (list (make-content-evidence token
                                        'gas-x86-syntax
                                        'declaration
                                        '(dialect architecture)
                                        60))]
          [(and directive? (member word '(.code16 .code16gcc .code32 .code64)))
           (list (make-content-evidence token
                                        'gas-x86-mode
                                        'distinctive
                                        '(dialect architecture)
                                        30))]
          [else
           fallback])]
       [(arm)
        (cond
          [(and directive? (eq? word '.syntax))
           (list (make-content-evidence token
                                        'gas-arm-syntax
                                        'declaration
                                        '(dialect architecture)
                                        60))]
          [(and directive? (member word '(.arm .thumb)))
           (list (make-content-evidence token
                                        'gas-arm-mode
                                        'distinctive
                                        '(dialect architecture)
                                        30))]
          [else
           fallback])]
       [(m68k)
        (cond
          [(asm-derived-token-has-tag? token 'gas-numeric-local-label)
           (list (make-content-evidence token
                                        'gas-m68k-local-label
                                        'distinctive
                                        '(dialect architecture)
                                        30))]
          [else
           fallback])]
       [(aarch64)
        (cond
          [(asm-derived-token-has-tag? token 'gas-relocation-modifier)
           (list (make-content-evidence token
                                        'gas-aarch64-relocation
                                        'distinctive
                                        '(dialect architecture)
                                        30))]
          [else
           fallback])]
       [else
        fallback])]
    [(nasm)
     (cond
       [(or (asm-derived-token-has-tag? token 'nasm-macro-open)
            (asm-derived-token-has-tag? token 'nasm-bits-mode))
        (list (make-content-evidence token
                                     'nasm-macro-or-bits
                                     'declaration
                                     '(dialect architecture)
                                     60))]
       [(or (asm-derived-token-has-tag? token 'nasm-macro-local)
            (asm-derived-token-has-tag? token 'nasm-macro-parameter)
            (asm-derived-token-has-tag? token 'nasm-token-concatenation))
        (list (make-content-evidence token
                                     'nasm-preprocessor
                                     'distinctive
                                     '(dialect)
                                     30))]
       [else
        fallback])]
    [else
     fallback]))

;; append-comment-span : source-span? (listof source-span?) -> (listof source-span?)
;;   Coalesce adjacent comment fragments so rival evidence can be compared by span.
(define (append-comment-span span spans)
  (cond
    [(null? spans)
     (list span)]
    [else
     (define previous
       (car spans))
     (cond
       [(= (source-span-end previous)
           (source-span-start span))
        (cons (source-span (source-span-start previous)
                           (source-span-end span))
              (cdr spans))]
       [else
        (cons span spans)])]))

;; scan-candidate : bytes? symbol? symbol? symbol? -> candidate-run?
;;   Run one shared profile scanner over the bounded input prefix.
(define (scan-candidate sample encoding dialect architecture)
  (define lexer
    (make-asm-derived-lexer #:dialect      dialect
                            #:architecture architecture
                            #:encoding     encoding))
  (define in
    (open-input-bytes sample))
  (let loop ([evidence '()]
             [comment-spans '()])
    (define token
      (lexer in))
    (cond
      [(eq? token 'eof)
       (candidate-run dialect
                      architecture
                      (reverse evidence)
                      (reverse comment-spans))]
      [(eq? (asm-derived-token-category token) 'comment)
       (loop evidence
             (append-comment-span (asm-derived-token-byte-span token)
                                  comment-spans))]
      [else
       (loop (append (reverse (profile-token-evidence dialect architecture token))
                     evidence)
             comment-spans)])))

;; span-contained? : source-span? source-span? -> boolean?
;;   Determine whether an evidence span lies entirely inside one comment span.
(define (span-contained? inner outer)
  (and (<= (source-span-start outer)
           (source-span-start inner))
       (<= (source-span-end inner)
           (source-span-end outer))))

;; strong-evidence? : asm-content-evidence? -> boolean?
;;   Restrict comment-rule conflicts to genuinely distinctive rival observations.
(define (strong-evidence? evidence)
  (and (unconditional-evidence? evidence)
       (>= (asm-content-evidence-contribution evidence) 30)))

;; candidate-conflicts : candidate-run? (listof candidate-run?) -> (listof asm-detection-conflict?)
;;   Identify rival strong evidence that this candidate interpreted as comments.
(define (candidate-conflicts run runs)
  (define-values (conflicts ignored-seen)
    (for/fold ([conflicts '()]
               [seen '()])
              ([rival (in-list runs)])
      (cond
        [(and (eq? (candidate-run-dialect run) (candidate-run-dialect rival))
              (eq? (candidate-run-architecture run)
                   (candidate-run-architecture rival)))
         (values conflicts seen)]
        [else
         (for/fold ([next-conflicts conflicts]
                    [next-seen seen])
                   ([evidence (in-list (candidate-run-evidence rival))])
           (define key
             (cons (candidate-run-dialect rival)
                   (asm-content-evidence-rule-id evidence)))
           (cond
             [(or (not (strong-evidence? evidence))
                  (member key next-seen)
                  (not (for/or ([span (in-list (candidate-run-comment-spans run))])
                         (span-contained? (asm-content-evidence-byte-span evidence)
                                          span))))
              (values next-conflicts next-seen)]
             [else
              (values
               (cons (asm-detection-conflict
                      (asm-content-evidence-rule-id evidence)
                      (candidate-run-dialect rival)
                      (asm-content-evidence-byte-span evidence)
                      (asm-content-evidence-context evidence))
                     next-conflicts)
               (cons key next-seen))]))])))
  (reverse conflicts))

;; unique-rule-evidence : (listof asm-content-evidence?) -> (listof asm-content-evidence?)
;;   Count a repeated observation only once within its scoring feature class.
(define (unique-rule-evidence evidence)
  (define-values (selected ignored-seen)
    (for/fold ([selected '()]
               [seen '()])
              ([item (in-list evidence)])
      (define key
        (cons (asm-content-evidence-feature-class item)
              (asm-content-evidence-rule-id item)))
      (cond
        [(or (not (unconditional-evidence? item))
             (member key seen))
         (values selected seen)]
        [else
         (values (cons item selected)
                 (cons key seen))])))
  (reverse selected))

;; feature-cap : symbol? -> exact-nonnegative-integer?
;;   Apply the published saturation limit for one independent evidence class.
(define (feature-cap feature-class)
  (case feature-class
    [(declaration) 60]
    [(distinctive) 60]
    [(instruction register) 80]
    [(convention) 10]
    [else 0]))

;; score-dimension : (listof asm-content-evidence?) (listof asm-detection-conflict?) symbol? -> exact-nonnegative-integer?
;;   Sum capped independent support and distinct strong conflicts for one dimension.
(define (score-dimension evidence conflicts dimension)
  (define usable
    (filter (lambda (item)
              (member dimension (asm-content-evidence-dimensions item)))
            (unique-rule-evidence evidence)))
  (define positive-score
    (for/sum ([feature-class (in-list '(declaration distinctive instruction register convention))])
      (min (feature-cap feature-class)
           (for/sum ([item (in-list usable)]
                     #:when (eq? (asm-content-evidence-feature-class item)
                                 feature-class))
             (asm-content-evidence-contribution item)))))
  (max 0
       (- positive-score
          (min 100 (* 50 (length conflicts))))))

;; candidate-from-run : candidate-run? (listof candidate-run?) -> asm-detection-candidate?
;;   Score one compatible tuple independently for dialect and architecture.
(define (candidate-from-run run runs)
  (define evidence
    (candidate-run-evidence run))
  (define conflicts
    (candidate-conflicts run runs))
  (define dialect-score
    (score-dimension evidence conflicts 'dialect))
  (define architecture-score
    (score-dimension evidence conflicts 'architecture))
  (define feature-classes
    (remove-duplicates
     (map asm-content-evidence-feature-class
          (unique-rule-evidence evidence))))
  (asm-detection-candidate (candidate-run-dialect run)
                           (candidate-run-architecture run)
                           dialect-score
                           architecture-score
                           (min dialect-score architecture-score)
                           feature-classes
                           evidence
                           conflicts))

;; candidate-before? : asm-detection-candidate? asm-detection-candidate? -> boolean?
;;   Establish deterministic ordering for equal-scoring compatible candidates.
(define (candidate-before? left right)
  (cond
    [(> (asm-detection-candidate-score left)
        (asm-detection-candidate-score right))
     #t]
    [(< (asm-detection-candidate-score left)
        (asm-detection-candidate-score right))
     #f]
    [else
     (string<? (format "~a/~a"
                       (asm-detection-candidate-dialect left)
                       (asm-detection-candidate-architecture left))
               (format "~a/~a"
                       (asm-detection-candidate-dialect right)
                       (asm-detection-candidate-architecture right)))]))

;; common-candidate-value : (listof asm-detection-candidate?) procedure? -> (or/c symbol? #f)
;;   Preserve a dimension only when every compatible candidate agrees on it.
(define (common-candidate-value candidates accessor)
  (cond
    [(null? candidates)
     #f]
    [else
     (define value
       (accessor (car candidates)))
     (cond
       [(for/and ([candidate (in-list (cdr candidates))])
          (eq? value (accessor candidate)))
        value]
       [else
        #f])]))

;; automatic-selection-enabled? : asm-detection-candidate? -> boolean?
;;   Gate automatic behavior until a candidate has a held-out calibration record.
(define (automatic-selection-enabled? candidate)
  (hash-ref calibrated-automatic-profiles
            (cons (asm-detection-candidate-dialect candidate)
                  (asm-detection-candidate-architecture candidate))
            #f))

;; select-probe-result : (listof asm-detection-candidate?) -> asm-auto-selection?
;;   Apply score bands while retaining compatible alternatives and calibration status.
(define (select-probe-result candidates)
  (define ordered
    (sort candidates candidate-before?))
  (define leading
    (cond
      [(pair? ordered)
       (car ordered)]
      [else
       #f]))
  (define leading-score
    (cond
      [leading
       (asm-detection-candidate-score leading)]
      [else
       0]))
  (define runner-score
    (cond
      [(pair? (cdr ordered))
       (asm-detection-candidate-score (cadr ordered))]
      [else
       0]))
  (define margin
    (- leading-score runner-score))
  (define compatible-candidates
    (cond
      [(zero? leading-score)
       '()]
      [else
       (filter (lambda (candidate)
                 (>= (asm-detection-candidate-score candidate)
                     (max 1 (- leading-score 14))))
               ordered)]))
  (define (dimension-candidates score-accessor)
    (define ordered-dimension
      (sort candidates
            (lambda (left right)
              (cond
                [(> (score-accessor left) (score-accessor right))
                 #t]
                [(< (score-accessor left) (score-accessor right))
                 #f]
                [else
                 (candidate-before? left right)]))))
    (define dimension-score
      (cond
        [(pair? ordered-dimension)
         (score-accessor (car ordered-dimension))]
        [else
         0]))
    (cond
      [(zero? dimension-score)
       '()]
      [else
       (filter (lambda (candidate)
                 (>= (score-accessor candidate)
                     (max 1 (- dimension-score 14))))
               ordered-dimension)]))
  (define dialect-candidates
    (dimension-candidates asm-detection-candidate-dialect-score))
  (define architecture-candidates
    (dimension-candidates asm-detection-candidate-architecture-score))
  (define independent-class-count
    (cond
      [leading
       (length (asm-detection-candidate-feature-classes leading))]
      [else
       0]))
  (define band
    (cond
      [(zero? leading-score)
       'unknown]
      [(pair? (asm-detection-candidate-conflicts leading))
       'conflicting]
      [(and (>= leading-score 80)
            (>= margin 20)
            (>= independent-class-count 2))
       'high]
      [(and (>= leading-score 60)
            (>= margin 15))
       'probable]
      [else
       'ambiguous]))
  (define automatic-enabled?
    (and leading
         (memq band '(high probable))
         (automatic-selection-enabled? leading)))
  (define reason
    (case band
      [(unknown)
       'insufficient-content-evidence]
      [(conflicting)
       'strong-rival-conflict]
      [(ambiguous)
       'insufficient-margin]
      [(high probable)
       (cond
         [automatic-enabled?
          'calibrated]
         [else
          'held-out-calibration-required])]
      [else
       'unknown]))
  (asm-auto-selection band
                      (cond
                        [(pair? compatible-candidates)
                         (common-candidate-value compatible-candidates
                                                 asm-detection-candidate-dialect)]
                        [else
                         (common-candidate-value dialect-candidates
                                                 asm-detection-candidate-dialect)])
                      (cond
                        [(pair? compatible-candidates)
                         (common-candidate-value compatible-candidates
                                                 asm-detection-candidate-architecture)]
                        [else
                         (common-candidate-value architecture-candidates
                                                 asm-detection-candidate-architecture)])
                      compatible-candidates
                      leading-score
                      margin
                      automatic-enabled?
                      reason))

;; probe-asm-bytes : bytes? [#:limit exact-positive-integer?] -> asm-probe?
;;   Probe a bounded byte prefix with every implemented shared profile scanner.
(define (probe-asm-bytes input #:limit [limit 32768])
  (unless (bytes? input)
    (raise-argument-error 'probe-asm-bytes "bytes?" input))
  (unless (exact-positive-integer? limit)
    (raise-argument-error 'probe-asm-bytes "exact-positive-integer?" limit))
  (define inspected-byte-count
    (min (bytes-length input) limit))
  (define sample
    (subbytes input 0 inspected-byte-count))
  (define-values (encoding ignored-encoding-evidence)
    (bom-evidence sample))
  (define source-encoding
    (cond
      [(eq? encoding 'unknown)
       'auto]
      [else
       encoding]))
  (define runs
    (for/list ([tuple (in-list (probe-tuples))])
      (scan-candidate sample source-encoding (car tuple) (cdr tuple))))
  (define candidates
    (sort (for/list ([run (in-list runs)])
            (candidate-from-run run runs))
          candidate-before?))
  (define selection
    (select-probe-result candidates))
  (define diagnostics
    (append
     (cond
       [(memq (asm-auto-selection-band selection) '(high probable))
        (cond
          [(asm-auto-selection-automatic-enabled? selection)
           '()]
          [else
           (list (profile-diagnostic 'automatic-selection-unqualified
                                     'info
                                     "The leading profile has not passed held-out calibration."))])]
       [(eq? (asm-auto-selection-band selection) 'unknown)
        (list (profile-diagnostic 'automatic-selection-unknown
                                  'info
                                  "The bounded source prefix has insufficient supported profile evidence."))]
       [else
        (list (profile-diagnostic 'automatic-selection-ambiguous
                                  'info
                                  "The bounded source prefix does not justify an exact profile selection."))])
     (cond
       [(< inspected-byte-count (bytes-length input))
        (list (profile-diagnostic 'inspection-truncated
                                  'info
                                  "Assembly probing stopped at its configured byte limit."))]
       [else
        '()])))
  (asm-probe 1
             selection
             candidates
             (apply append
                    (map candidate-run-evidence runs))
             diagnostics
             encoding
             inspected-byte-count
             (< inspected-byte-count (bytes-length input))))

(module+ test
  (require rackunit)

  (define generic-selection
    (resolve-asm-selection #:dialect 'generic))
  (define ca65-selection
    (resolve-asm-selection #:dialect 'ca65))
  (define gas-without-target-selection
    (resolve-asm-selection #:dialect 'gas))
  (define gas-x86-selection
    (resolve-asm-selection #:dialect 'gas #:architecture 'x86))
  (define gas-unsupported-target-selection
    (resolve-asm-selection #:dialect 'gas #:architecture 'riscv))
  (define utf-16-detection
    (detect-asm-bytes #"\377\376l\0d\0a\0" #:dialect 'generic))
  (define gas-detection
    (detect-asm-bytes #"mov rax, 1" #:dialect 'gas #:architecture 'x86))
  (define truncated-detection
    (detect-asm-bytes #"abcdef" #:limit 3))
  (define nasm-probe
    (probe-asm-bytes #"%macro m 1\n%%local: mov rax, %1\n%endmacro\nBITS 64\n"))
  (define gas-probe
    (probe-asm-bytes #".intel_syntax noprefix\n.code64\nmov rax, 1\n"))
  (define shared-m68k-probe
    (probe-asm-bytes #"move.l d0,(a0)\n"))
  (define comment-and-string-probe
    (probe-asm-bytes #"; .setcpu \"6502\"\n\".setcpu \\\"6502\\\"\"\n"))
  (define macro-contained-setting-probe
    (probe-asm-bytes #".macro m\n.setcpu \"6502\"\n.endmacro\n"))
  (define truncated-probe
    (probe-asm-bytes #".intel_syntax noprefix\n" #:limit 8))
  (define comment-swallow-probe
    (probe-asm-bytes #"| !cpu 6510\n!zone main\n"))

  ;; probe-has-rule? : asm-probe? symbol? -> boolean?
  ;;   Determine whether a bounded probe retained an expected scanner rule.
  (define (probe-has-rule? probe rule-id)
    (ormap (lambda (evidence)
             (eq? (asm-content-evidence-rule-id evidence) rule-id))
           (asm-probe-evidence probe)))

  ;; find-probe-candidate : asm-probe? symbol? symbol? -> (or/c asm-detection-candidate? #f)
  ;;   Find one named candidate in a deterministic bounded-probe report.
  (define (find-probe-candidate probe dialect architecture)
    (findf (lambda (candidate)
             (and (eq? (asm-detection-candidate-dialect candidate) dialect)
                  (eq? (asm-detection-candidate-architecture candidate)
                       architecture)))
           (asm-probe-candidates probe)))

  (check-equal? (asm-selection-effective generic-selection) 'generic)
  (check-equal? (asm-selection-origin generic-selection) 'explicit)
  (check-equal? (asm-selection-effective ca65-selection) 'ca65)
  (check-false (asm-selection-effective gas-without-target-selection))
  (check-equal? (asm-diagnostic-code
                 (car (asm-selection-diagnostics gas-without-target-selection)))
                'architecture-required)
  (check-equal? (asm-selection-effective gas-x86-selection) 'gas)
  (check-false (asm-selection-effective gas-unsupported-target-selection))
  (check-equal? (asm-diagnostic-code
                 (car (asm-selection-diagnostics gas-unsupported-target-selection)))
                'unsupported-architecture)
  (check-equal? (asm-detection-schema-version utf-16-detection) 1)
  (check-equal? (asm-detection-encoding utf-16-detection) 'utf-16le)
  (check-equal? (asm-evidence-dimension (car (asm-detection-evidence utf-16-detection)))
                'encoding)
  (check-equal? (asm-selection-effective (asm-detection-selection gas-detection))
                'gas)
  (check-true (asm-detection-truncated? truncated-detection))
  (check-equal? (asm-detection-inspected-byte-count truncated-detection) 3)
  (check-equal? (asm-auto-selection-band (asm-probe-selection nasm-probe))
                'probable)
  (check-equal? (asm-auto-selection-dialect (asm-probe-selection nasm-probe))
                'nasm)
  (check-equal? (asm-auto-selection-architecture (asm-probe-selection nasm-probe))
                'x86)
  (check-false (asm-auto-selection-automatic-enabled?
                (asm-probe-selection nasm-probe)))
  (check-equal? (asm-auto-selection-reason (asm-probe-selection nasm-probe))
                'held-out-calibration-required)
  (check-true (probe-has-rule? nasm-probe 'nasm-macro-or-bits))
  (check-equal? (asm-auto-selection-band (asm-probe-selection gas-probe))
                'high)
  (check-equal? (asm-auto-selection-dialect (asm-probe-selection gas-probe))
                'gas)
  (check-equal? (asm-auto-selection-architecture (asm-probe-selection gas-probe))
                'x86)
  (check-false (asm-auto-selection-automatic-enabled?
                (asm-probe-selection gas-probe)))
  (check-true (probe-has-rule? gas-probe 'gas-x86-syntax))
  (check-equal? (asm-auto-selection-band (asm-probe-selection shared-m68k-probe))
                'unknown)
  (check-false (asm-auto-selection-dialect
                (asm-probe-selection shared-m68k-probe)))
  (check-equal? (asm-auto-selection-architecture
                 (asm-probe-selection shared-m68k-probe))
                'm68k)
  (check-equal? (asm-auto-selection-band
                 (asm-probe-selection comment-and-string-probe))
                'unknown)
  (check-equal? (asm-auto-selection-band
                 (asm-probe-selection macro-contained-setting-probe))
                'unknown)
  (check-true (asm-probe-truncated? truncated-probe))
  (check-equal? (asm-probe-inspected-byte-count truncated-probe) 8)
  (define gas-m68k-comment-candidate
    (find-probe-candidate comment-swallow-probe 'gas 'm68k))
  (check-not-false gas-m68k-comment-candidate)
  (check-true
   (pair? (asm-detection-candidate-conflicts gas-m68k-comment-candidate))))
