#lang racket/base

;;;
;;; String Compatibility Helpers
;;;
;;
;; Allocation-free helpers for comparing a string region with another string.

;; substring=?    : string? exact-nonnegative-integer? string? -> boolean?
;;   Determine whether source contains target at start, without allocating.
;; substring-ci=? : string? exact-nonnegative-integer? string? -> boolean?
;;   Determine whether source contains target at start, case-insensitively and
;;   without allocating.

(provide substring=?
         substring-ci=?)

;; substring=? : string? exact-nonnegative-integer? string? -> boolean?
;;   Determine whether source contains target at start, without allocating.
(define (substring=? source start target)
  (define source-len (string-length source))
  (define target-len (string-length target))
  (cond
    [(> (+ start target-len) source-len) #f]
    [else
     (let loop ([i 0])
       (cond
         [(= i target-len) #t]
         [(char=? (string-ref source (+ start i))
                  (string-ref target i))
          (loop (add1 i))]
         [else #f]))]))

;; substring-ci=? : string? exact-nonnegative-integer? string? -> boolean?
;;   Determine whether source contains target at start, case-insensitively and
;;   without allocating.
(define (substring-ci=? source start target)
  (define source-len (string-length source))
  (define target-len (string-length target))
  (cond
    [(> (+ start target-len) source-len) #f]
    [else
     (let loop ([i 0])
       (cond
         [(= i target-len) #t]
         [(char-ci=? (string-ref source (+ start i))
                     (string-ref target i))
          (loop (add1 i))]
         [else #f]))]))
