#lang racket/base

;;;
;;; TOML Projection
;;;

;; Project derived TOML tokens into the reusable stream model.

;; project-toml-derived-token : (or/c toml-derived-token? 'eof) toml-config? -> token-like?
;;   Convert a derived TOML token into a reusable-stream token-like value.

(provide project-toml-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt"
         "toml-derived.rkt")

;; derived->stream-category : toml-derived-token? -> symbol?
;;   Choose a reusable category from TOML-derived tags.
(define (derived->stream-category token)
  (cond
    [(toml-derived-token-has-tag? token 'malformed-token) stream-category-unknown]
    [(toml-derived-token-has-tag? token 'comment) stream-category-comment]
    [(toml-derived-token-has-tag? token 'whitespace) stream-category-whitespace]
    [(toml-derived-token-has-tag? token 'literal) stream-category-literal]
    [(toml-derived-token-has-tag? token 'operator) stream-category-operator]
    [(toml-derived-token-has-tag? token 'delimiter) stream-category-delimiter]
    [else stream-category-identifier]))

;; project-toml-derived-token : (or/c toml-derived-token? 'eof) toml-config? -> token-like?
;;   Convert a derived TOML token into the reusable stream model.
(define (project-toml-derived-token token config)
  (cond
    [(eq? token 'eof)
     (wrap-token-with-pos 'eof
                          (make-stream-position 1 1 0)
                          (make-stream-position 1 1 0)
                          (toml-config-source-positions config))]
    [(toml-derived-token-has-tag? token 'malformed-token)
     (case (toml-config-errors config)
       [(emit-unknown)
        (wrap-token-with-pos (make-stream-token stream-category-unknown (toml-derived-token-text token))
                             (toml-derived-token-start token)
                             (toml-derived-token-end token)
                             (toml-config-source-positions config))]
       [(raise)
        (define start (toml-derived-token-start token))
        (define end (toml-derived-token-end token))
        (raise-read-error "unknown TOML input" #f
                          (position-line start) (position-col start) (position-offset start)
                          (- (position-offset end) (position-offset start)))])]
    [(and (eq? (toml-config-trivia config) 'skip)
          (member (derived->stream-category token) '(comment whitespace)))
     #f]
    [else
     (wrap-token-with-pos (make-stream-token (derived->stream-category token)
                                              (toml-derived-token-text token))
                          (toml-derived-token-start token)
                          (toml-derived-token-end token)
                          (toml-config-source-positions config))]))
