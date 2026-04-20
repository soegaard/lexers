#lang racket/base

;;;
;;; TSV Projection
;;;
;;
;; Project derived TSV tokens into the reusable stream model.

;; project-tsv-derived-token : (or/c delimited-derived-token? 'eof) tsv-config? -> token-like?
;;   Convert a derived TSV token into a reusable-stream token-like value.

(provide project-tsv-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "delimited-derived.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt")

;; skip-trivia? : tsv-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (tsv-config-trivia config) 'skip))

;; derived->stream-category : delimited-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(delimited-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(delimited-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(delimited-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [(delimited-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [else
     stream-category-unknown]))

;; raw-eof->token : tsv-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (tsv-config-source-positions config)))

;; malformed-token->result : delimited-derived-token? tsv-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (tsv-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (delimited-derived-token-text derived-token))
      (delimited-derived-token-start derived-token)
      (delimited-derived-token-end derived-token)
      (tsv-config-source-positions config))]
    [(raise)
     (define start-pos
       (delimited-derived-token-start derived-token))
     (define end-pos
       (delimited-derived-token-end derived-token))
     (raise-read-error "unknown TSV input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-tsv-derived-token
            "unsupported TSV error policy: ~a"
            (tsv-config-errors config))]))

;; visible-derived-token? : delimited-derived-token? tsv-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : delimited-derived-token? tsv-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (delimited-derived-token-text derived-token))
   (delimited-derived-token-start derived-token)
   (delimited-derived-token-end derived-token)
   (tsv-config-source-positions config)))

;; project-tsv-derived-token : (or/c delimited-derived-token? 'eof) tsv-config? -> token-like?
;;   Convert a derived TSV token into the reusable stream model.
(define (project-tsv-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(delimited-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
