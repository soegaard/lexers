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
;; resolve-asm-selection       : keyword-arguments -> asm-selection?
;;   Resolve an explicit profile and, when needed, its target architecture.
;; detect-asm-bytes            : bytes? keyword-arguments -> asm-detection?
;;   Inspect BOM evidence only; automatic dialect selection remains disabled.

(provide (struct-out asm-selection)
         (struct-out asm-evidence)
         (struct-out asm-detection)
         resolve-asm-selection
         detect-asm-bytes)

(require (only-in "../asm.rkt"
                  asm-diagnostic
                  asm-diagnostic-code
                  asm-profile-architecture
                  asm-profile-status
                  asm-profiles)
         (only-in "../private/source.rkt"
                  source-span)
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
  (check-equal? (asm-detection-inspected-byte-count truncated-detection) 3))
