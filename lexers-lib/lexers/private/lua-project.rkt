#lang racket/base

;;;
;;; Lua Projection
;;;

;; Project derived Lua tokens into the reusable stream model.

(provide project-lua-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt"
         "lua-derived.rkt")

;; project-lua-derived-token : (or/c lua-derived-token? 'eof) lua-config? -> token-like?
;;   Convert a derived Lua token into a reusable-stream token-like value.
(define (project-lua-derived-token token config)
  (define category
    (cond [(and (not (eq? token 'eof)) (lua-derived-token-has-tag? token 'malformed-token)) stream-category-unknown]
          [(and (not (eq? token 'eof)) (lua-derived-token-has-tag? token 'comment)) stream-category-comment]
          [(and (not (eq? token 'eof)) (lua-derived-token-has-tag? token 'whitespace)) stream-category-whitespace]
          [(and (not (eq? token 'eof)) (lua-derived-token-has-tag? token 'keyword)) stream-category-keyword]
          [(and (not (eq? token 'eof)) (lua-derived-token-has-tag? token 'literal)) stream-category-literal]
          [(and (not (eq? token 'eof)) (lua-derived-token-has-tag? token 'operator)) stream-category-operator]
          [(and (not (eq? token 'eof)) (lua-derived-token-has-tag? token 'delimiter)) stream-category-delimiter]
          [else stream-category-identifier]))
  (cond [(eq? token 'eof)
         (wrap-token-with-pos 'eof (make-stream-position 1 1 0) (make-stream-position 1 1 0)
                              (lua-config-source-positions config))]
        [(eq? category stream-category-unknown)
         (case (lua-config-errors config)
           [(emit-unknown) (wrap-token-with-pos (make-stream-token category (lua-derived-token-text token))
                                                 (lua-derived-token-start token) (lua-derived-token-end token)
                                                 (lua-config-source-positions config))]
           [(raise)
            (define start (lua-derived-token-start token))
            (define end (lua-derived-token-end token))
            (raise-read-error "unknown Lua input" #f (position-line start) (position-col start)
                              (position-offset start) (- (position-offset end) (position-offset start)))])]
        [(and (eq? (lua-config-trivia config) 'skip)
              (member category '(comment whitespace))) #f]
        [else (wrap-token-with-pos (make-stream-token category (lua-derived-token-text token))
                                   (lua-derived-token-start token) (lua-derived-token-end token)
                                   (lua-config-source-positions config))]))
