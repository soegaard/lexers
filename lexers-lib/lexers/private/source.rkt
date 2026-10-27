#lang racket/base

;;;
;;; Incremental Source Reader
;;;

;; Incremental byte decoding and byte/character position tracking for lexers
;; that must retain the original source bytes.

;; source-span?                 : any/c -> boolean?
;;   Recognize a zero-based, half-open byte span.
;; source-span-start            : source-span? -> exact-nonnegative-integer?
;;   Extract a byte span's inclusive starting offset.
;; source-span-end              : source-span? -> exact-nonnegative-integer?
;;   Extract a byte span's exclusive ending offset.
;; source-unit?                 : any/c -> boolean?
;;   Recognize one decoded source unit.
;; source-unit-raw-bytes        : source-unit? -> immutable-bytes?
;;   Extract the exact bytes owned by one source unit.
;; source-unit-text             : source-unit? -> string?
;;   Extract the decoded text, with U+FFFD for malformed input.
;; source-unit-byte-span        : source-unit? -> source-span?
;;   Extract the byte span covered by one source unit.
;; source-unit-start            : source-unit? -> position?
;;   Extract the parser-tools-compatible starting position.
;; source-unit-end              : source-unit? -> position?
;;   Extract the parser-tools-compatible ending position.
;; source-unit-encoding         : source-unit? -> symbol?
;;   Extract the encoding used for one source unit.
;; source-unit-malformed?       : source-unit? -> boolean?
;;   Determine whether a unit represents malformed encoded input.
;; source-unit-bom?             : source-unit? -> boolean?
;;   Determine whether a unit represents a byte-order mark.
;; source-reader?               : any/c -> boolean?
;;   Recognize an incremental source reader.
;; make-source-reader           : keyword-arguments -> source-reader?
;;   Construct a reader for UTF-8 or BOM-selected UTF-16 input.
;; source-read-unit!            : source-reader? input-port? -> (or/c source-unit? eof-object?)
;;   Read one bounded source unit while retaining exact bytes and positions.
;; source-reader-encoding       : source-reader? -> symbol?
;;   Extract the resolved source encoding, or 'auto before the first unit.
;; source-reader-checkpoint     : source-reader? -> source-checkpoint?
;;   Capture the reader's position and pending-byte state.
;; source-reader-restore!       : source-reader? source-checkpoint? -> void?
;;   Restore a checkpoint before reading a port positioned at its byte offset.

(provide (struct-out source-span)
         (struct-out source-unit)
         source-reader?
         make-source-reader
         source-read-unit!
         source-reader-encoding
         source-reader-checkpoint
         source-reader-restore!)

(require parser-tools/lex
         "parser-tools-compat.rkt")

;; A zero-based, half-open byte span in the authoritative source.
(struct source-span (start end) #:transparent)

;; One immutable source fragment.  A unit is at most one decoded scalar value.
(struct source-unit (raw-bytes text byte-span start end encoding malformed? bom?)
  #:transparent)

;; A restorable source-reader state.  Restoring requires a new input port
;; positioned at this checkpoint's byte offset; this structure does not seek.
(struct source-checkpoint (encoding pending byte-offset line column character-offset
                                    previous-carriage-return?)
  #:transparent)

;; Mutable state for a bounded source reader.  `pending` only holds decoder
;; lookahead: at most three bytes for UTF-8 and two for UTF-16.
(struct source-reader (encoding pending byte-offset line column character-offset
                                 previous-carriage-return? input-port)
  #:mutable
  #:transparent)

;; validate-encoding : symbol? -> symbol?
;;   Validate one supported source encoding request.
(define (validate-encoding encoding)
  (case encoding
    [(auto utf-8 utf-16le utf-16be) encoding]
    [else
     (raise-argument-error 'make-source-reader
                           "(or/c 'auto 'utf-8 'utf-16le 'utf-16be)"
                           encoding)]))

;; make-source-reader : [#:encoding symbol?] -> source-reader?
;;   Construct an incremental byte reader with byte and character positions.
(define (make-source-reader #:encoding [encoding 'auto])
  (source-reader (validate-encoding encoding)
                 '()
                 0
                 1
                 0
                 1
                 #f
                 #f))

;; source-reader-checkpoint : source-reader? -> source-checkpoint?
;;   Capture one reader state at an exact input boundary.
(define (source-reader-checkpoint reader)
  (unless (null? (source-reader-pending reader))
    (raise-arguments-error 'source-reader-checkpoint
                           "cannot checkpoint while decoder lookahead is pending"))
  (source-checkpoint (source-reader-encoding reader)
                     (source-reader-pending reader)
                     (source-reader-byte-offset reader)
                     (source-reader-line reader)
                     (source-reader-column reader)
                     (source-reader-character-offset reader)
                     (source-reader-previous-carriage-return? reader)))

;; source-reader-restore! : source-reader? source-checkpoint? -> void?
;;   Restore a reader before supplying a port positioned at the checkpoint.
(define (source-reader-restore! reader checkpoint)
  (unless (source-reader? reader)
    (raise-argument-error 'source-reader-restore! "source-reader?" reader))
  (unless (source-checkpoint? checkpoint)
    (raise-argument-error 'source-reader-restore! "source-checkpoint?" checkpoint))
  (set-source-reader-encoding! reader (source-checkpoint-encoding checkpoint))
  (set-source-reader-pending! reader (source-checkpoint-pending checkpoint))
  (set-source-reader-byte-offset! reader (source-checkpoint-byte-offset checkpoint))
  (set-source-reader-line! reader (source-checkpoint-line checkpoint))
  (set-source-reader-column! reader (source-checkpoint-column checkpoint))
  (set-source-reader-character-offset!
   reader
   (source-checkpoint-character-offset checkpoint))
  (set-source-reader-previous-carriage-return?!
   reader
   (source-checkpoint-previous-carriage-return? checkpoint))
  (set-source-reader-input-port! reader #f))

;; check-input-port! : source-reader? input-port? -> void?
;;   Ensure one reader is used with exactly one input port.
(define (check-input-port! reader in)
  (unless (input-port? in)
    (raise-argument-error 'source-read-unit! "input-port?" in))
  (define prior-port
    (source-reader-input-port reader))
  (cond
    [(not prior-port)
     (set-source-reader-input-port! reader in)]
    [(eq? prior-port in)
     (void)]
    [else
     (raise-arguments-error 'source-read-unit!
                            "a source reader cannot switch input ports"
                            "initial port" prior-port
                            "given port" in)]))

;; take-byte! : source-reader? input-port? -> (or/c byte? eof-object?)
;;   Consume one byte, first using the bounded decoder lookahead queue.
(define (take-byte! reader in)
  (define pending
    (source-reader-pending reader))
  (cond
    [(pair? pending)
     (set-source-reader-pending! reader (cdr pending))
     (car pending)]
    [else
     (read-byte in)]))

;; put-back-bytes! : source-reader? (listof byte?) -> void?
;;   Return decoder lookahead to the front of the bounded pending-byte queue.
(define (put-back-bytes! reader byte-list)
  (set-source-reader-pending!
   reader
   (append byte-list (source-reader-pending reader))))

;; current-position : source-reader? -> position?
;;   Construct a parser-tools-compatible position from reader state.
(define (current-position reader)
  (make-stream-position (source-reader-character-offset reader)
                        (source-reader-line reader)
                        (source-reader-column reader)))

;; advance-position! : source-reader? string? -> void?
;;   Advance character positions while treating CRLF as one logical line break.
(define (advance-position! reader text)
  (for ([ch (in-string text)])
    (set-source-reader-character-offset!
     reader
     (add1 (source-reader-character-offset reader)))
    (cond
      [(char=? ch #\return)
       (set-source-reader-line! reader (add1 (source-reader-line reader)))
       (set-source-reader-column! reader 0)
       (set-source-reader-previous-carriage-return?! reader #t)]
      [(char=? ch #\newline)
       (cond
         [(source-reader-previous-carriage-return? reader)
          (set-source-reader-previous-carriage-return?! reader #f)]
         [else
          (set-source-reader-line! reader (add1 (source-reader-line reader)))
          (set-source-reader-column! reader 0)])]
      [else
       (set-source-reader-column! reader (add1 (source-reader-column reader)))
       (set-source-reader-previous-carriage-return?! reader #f)])))

;; make-unit! : source-reader? bytes? string? symbol? boolean? boolean? -> source-unit?
;;   Create a byte-owning unit and advance the reader's source positions.
(define (make-unit! reader raw text encoding malformed? bom?)
  (define start-byte
    (source-reader-byte-offset reader))
  (define start-position
    (current-position reader))
  (define owned-bytes
    (bytes->immutable-bytes raw))
  (set-source-reader-byte-offset!
   reader
   (+ start-byte (bytes-length owned-bytes)))
  (advance-position! reader text)
  (source-unit owned-bytes
               text
               (source-span start-byte (source-reader-byte-offset reader))
               start-position
               (current-position reader)
               encoding
               malformed?
               bom?))

;; continuation-byte? : byte? -> boolean?
;;   Determine whether a byte has the UTF-8 continuation-byte shape.
(define (continuation-byte? byte)
  (<= #x80 byte #xBF))

;; bytes-from-list : (listof byte?) -> immutable-bytes?
;;   Construct immutable bytes from a small decoder lookahead list.
(define (bytes-from-list byte-list)
  (bytes->immutable-bytes (apply bytes byte-list)))

;; utf-8-code-point : byte? (listof byte?) -> exact-nonnegative-integer?
;;   Decode one already-validated UTF-8 scalar value.
(define (utf-8-code-point first rest)
  (case (length rest)
    [(1)
     (bitwise-ior (arithmetic-shift (bitwise-and first #x1F) 6)
                  (bitwise-and (car rest) #x3F))]
    [(2)
     (bitwise-ior (arithmetic-shift (bitwise-and first #x0F) 12)
                  (arithmetic-shift (bitwise-and (car rest) #x3F) 6)
                  (bitwise-and (cadr rest) #x3F))]
    [(3)
     (bitwise-ior (arithmetic-shift (bitwise-and first #x07) 18)
                  (arithmetic-shift (bitwise-and (car rest) #x3F) 12)
                  (arithmetic-shift (bitwise-and (cadr rest) #x3F) 6)
                  (bitwise-and (caddr rest) #x3F))]
    [else
     (error 'utf-8-code-point "expected one to three continuation bytes")]))

;; valid-utf-8-sequence? : byte? (listof byte?) -> boolean?
;;   Check UTF-8 continuation shapes and the scalar-value range restrictions.
(define (valid-utf-8-sequence? first rest)
  (and (for/and ([byte (in-list rest)])
         (continuation-byte? byte))
       (cond
         [(= (length rest) 1)
          (<= #xC2 first #xDF)]
         [(= (length rest) 2)
          (and (<= #xE0 first #xEF)
               (or (not (= first #xE0)) (>= (car rest) #xA0))
               (or (not (= first #xED)) (<= (car rest) #x9F)))]
         [(= (length rest) 3)
          (and (<= #xF0 first #xF4)
               (or (not (= first #xF0)) (>= (car rest) #x90))
               (or (not (= first #xF4)) (<= (car rest) #x8F)))]
         [else
          #f])))

;; utf-8-width : byte? -> exact-nonnegative-integer?
;;   Return the expected UTF-8 unit width, or zero for an invalid lead byte.
(define (utf-8-width first)
  (cond
    [(<= first #x7F) 1]
    [(<= #xC2 first #xDF) 2]
    [(<= #xE0 first #xEF) 3]
    [(<= #xF0 first #xF4) 4]
    [else 0]))

;; read-following-bytes! : source-reader? input-port? exact-nonnegative-integer? -> (listof byte?)
;;   Consume up to count bytes for one bounded decoder decision.
(define (read-following-bytes! reader in count)
  (let loop ([remaining count]
             [result '()])
    (cond
      [(zero? remaining)
       (reverse result)]
      [else
       (define next
         (take-byte! reader in))
       (cond
         [(eof-object? next)
          (reverse result)]
         [else
          (loop (sub1 remaining) (cons next result))])])))

;; read-utf-8-unit! : source-reader? input-port? byte? -> source-unit?
;;   Decode one UTF-8 unit or preserve one malformed byte as U+FFFD.
(define (read-utf-8-unit! reader in first)
  (define width
    (utf-8-width first))
  (cond
    [(= width 1)
     (make-unit! reader
                 (bytes first)
                 (string (integer->char first))
                 'utf-8
                 #f
                 #f)]
    [(zero? width)
     (make-unit! reader (bytes first) "\uFFFD" 'utf-8 #t #f)]
    [else
     (define rest
       (read-following-bytes! reader in (sub1 width)))
     (cond
       [(and (= (length rest) (sub1 width))
             (valid-utf-8-sequence? first rest))
        (define code-point
          (utf-8-code-point first rest))
        (make-unit! reader
                    (bytes-from-list (cons first rest))
                    (string (integer->char code-point))
                    'utf-8
                    #f
                    (and (zero? (source-reader-byte-offset reader))
                         (= code-point #xFEFF)))]
       [else
        (put-back-bytes! reader rest)
        (make-unit! reader (bytes first) "\uFFFD" 'utf-8 #t #f)])]))

;; bytes->utf-16-unit : byte? byte? symbol? -> exact-nonnegative-integer?
;;   Decode one UTF-16 code unit according to the resolved byte order.
(define (bytes->utf-16-unit first second encoding)
  (case encoding
    [(utf-16le)
     (+ first (arithmetic-shift second 8))]
    [(utf-16be)
     (+ second (arithmetic-shift first 8))]
    [else
     (error 'bytes->utf-16-unit "expected a UTF-16 encoding")]))

;; utf-16-high-surrogate? : exact-nonnegative-integer? -> boolean?
;;   Recognize one UTF-16 high surrogate code unit.
(define (utf-16-high-surrogate? value)
  (<= #xD800 value #xDBFF))

;; utf-16-low-surrogate? : exact-nonnegative-integer? -> boolean?
;;   Recognize one UTF-16 low surrogate code unit.
(define (utf-16-low-surrogate? value)
  (<= #xDC00 value #xDFFF))

;; read-utf-16-unit! : source-reader? input-port? byte? symbol? -> source-unit?
;;   Decode one UTF-16 scalar value or retain malformed code-unit bytes.
(define (read-utf-16-unit! reader in first encoding)
  (define second
    (take-byte! reader in))
  (cond
    [(eof-object? second)
     (make-unit! reader (bytes first) "\uFFFD" encoding #t #f)]
    [else
     (define first-unit
       (bytes->utf-16-unit first second encoding))
     (cond
       [(utf-16-high-surrogate? first-unit)
        (define trail
          (read-following-bytes! reader in 2))
        (cond
          [(= (length trail) 2)
           (define low-unit
             (bytes->utf-16-unit (car trail) (cadr trail) encoding))
           (cond
             [(utf-16-low-surrogate? low-unit)
              (define code-point
                (+ #x10000
                   (arithmetic-shift (- first-unit #xD800) 10)
                   (- low-unit #xDC00)))
              (make-unit! reader
                          (bytes-from-list (list first second (car trail) (cadr trail)))
                          (string (integer->char code-point))
                          encoding
                          #f
                          #f)]
             [else
              (put-back-bytes! reader trail)
              (make-unit! reader (bytes first second) "\uFFFD" encoding #t #f)])]
          [else
           (put-back-bytes! reader trail)
           (make-unit! reader (bytes first second) "\uFFFD" encoding #t #f)])]
       [(utf-16-low-surrogate? first-unit)
        (make-unit! reader (bytes first second) "\uFFFD" encoding #t #f)]
       [else
        (make-unit! reader
                    (bytes first second)
                    (string (integer->char first-unit))
                    encoding
                    #f
                    (and (zero? (source-reader-byte-offset reader))
                         (= first-unit #xFEFF)))])]))

;; resolve-auto-encoding! : source-reader? input-port? byte? -> (or/c source-unit? byte?)
;;   Resolve an initial BOM.  A returned byte still needs UTF-8 decoding.
(define (resolve-auto-encoding! reader in first)
  (cond
    [(= first #xFF)
     (define second
       (take-byte! reader in))
     (cond
       [(and (byte? second) (= second #xFE))
        (set-source-reader-encoding! reader 'utf-16le)
        (make-unit! reader (bytes first second) "\uFEFF" 'utf-16le #f #t)]
       [else
        (set-source-reader-encoding! reader 'utf-8)
        (when (byte? second)
          (put-back-bytes! reader (list second)))
        first])]
    [(= first #xFE)
     (define second
       (take-byte! reader in))
     (cond
       [(and (byte? second) (= second #xFF))
        (set-source-reader-encoding! reader 'utf-16be)
        (make-unit! reader (bytes first second) "\uFEFF" 'utf-16be #f #t)]
       [else
        (set-source-reader-encoding! reader 'utf-8)
        (when (byte? second)
          (put-back-bytes! reader (list second)))
        first])]
    [(= first #xEF)
     (define rest
       (read-following-bytes! reader in 2))
     (cond
       [(equal? rest (list #xBB #xBF))
        (set-source-reader-encoding! reader 'utf-8)
        (make-unit! reader (bytes first #xBB #xBF) "\uFEFF" 'utf-8 #f #t)]
       [else
        (set-source-reader-encoding! reader 'utf-8)
        (put-back-bytes! reader rest)
        first])]
    [else
     (set-source-reader-encoding! reader 'utf-8)
     first]))

;; source-read-unit! : source-reader? input-port? -> (or/c source-unit? eof-object?)
;;   Decode one bounded source unit while preserving original bytes and spans.
(define (source-read-unit! reader in)
  (unless (source-reader? reader)
    (raise-argument-error 'source-read-unit! "source-reader?" reader))
  (check-input-port! reader in)
  (define first
    (take-byte! reader in))
  (cond
    [(eof-object? first)
     eof]
    [(eq? (source-reader-encoding reader) 'auto)
     (define resolved
       (resolve-auto-encoding! reader in first))
     (cond
       [(source-unit? resolved)
        resolved]
       [else
        (read-utf-8-unit! reader in resolved)])]
    [(eq? (source-reader-encoding reader) 'utf-8)
     (read-utf-8-unit! reader in first)]
    [else
     (read-utf-16-unit! reader in first (source-reader-encoding reader))]))

(module+ test
  (require rackunit
           racket/list)

  ;; read-all-units : bytes? -> (listof source-unit?)
  ;;   Decode all source units from immutable byte input.
  (define (read-all-units input)
    (define reader
      (make-source-reader))
    (define in
      (open-input-bytes input))
    (let loop ([units '()])
      (define result
        (source-read-unit! reader in))
      (cond
        [(eof-object? result)
         (reverse units)]
        [else
         (loop (cons result units))])))

  (define utf-8-units
    (read-all-units #"alpha\r\nbeta\n"))
  (define utf-16le-units
    (read-all-units #"\377\376a\0b\0"))
  (define malformed-units
    (read-all-units #"a\303(b"))
  (define checkpoint-reader
    (make-source-reader #:encoding 'utf-8))
  (define checkpoint-first
    (source-read-unit! checkpoint-reader (open-input-bytes #"abc")))
  (define checkpoint
    (source-reader-checkpoint checkpoint-reader))
  (source-reader-restore! checkpoint-reader checkpoint)
  (define checkpoint-next
    (source-read-unit! checkpoint-reader (open-input-bytes #"bc")))
  (define cr-unit
    (list-ref utf-8-units 5))
  (define lf-unit
    (list-ref utf-8-units 6))

  (check-equal? (apply bytes-append (map source-unit-raw-bytes utf-8-units))
                #"alpha\r\nbeta\n")
  (check-equal? (map source-unit-text utf-16le-units)
                '("\uFEFF" "a" "b"))
  (check-equal? (map source-unit-encoding utf-16le-units)
                '(utf-16le utf-16le utf-16le))
  (check-true (source-unit-bom? (car utf-16le-units)))
  (check-equal? (apply bytes-append (map source-unit-raw-bytes utf-16le-units))
                #"\377\376a\0b\0")
  (check-true (source-unit-malformed? (cadr malformed-units)))
  (check-equal? (apply bytes-append (map source-unit-raw-bytes malformed-units))
                #"a\303(b")
  (check-equal? (position-line (source-unit-start cr-unit)) 1)
  (check-equal? (position-line (source-unit-end cr-unit)) 2)
  (check-equal? (position-line (source-unit-start lf-unit)) 2)
  (check-equal? (position-line (source-unit-end lf-unit)) 2)
  (check-equal? (source-span-start (source-unit-byte-span (car utf-8-units))) 0)
  (check-equal? (source-span-end (source-unit-byte-span (last utf-8-units))) 12)
  (check-equal? (source-unit-text checkpoint-first) "a")
  (check-equal? (source-unit-text checkpoint-next) "b")
  (check-equal? (source-span-start (source-unit-byte-span checkpoint-next)) 1))
