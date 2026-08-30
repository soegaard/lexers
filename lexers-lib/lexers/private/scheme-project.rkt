#lang racket/base

;;;
;;; Scheme Projection
;;;

(provide project-scheme-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt"
         "scheme-derived.rkt")

(define (project-scheme-derived-token token config)
  (cond [(eq? token 'eof)
         (wrap-token-with-pos 'eof
                              (make-stream-position 1 1 0)
                              (make-stream-position 1 1 0)
                              (scheme-config-source-positions config))]
        [else
         (define category
           (cond [(scheme-derived-token-has-tag? token 'malformed-token) stream-category-unknown]
                 [(scheme-derived-token-has-tag? token 'comment) stream-category-comment]
                 [(scheme-derived-token-has-tag? token 'whitespace) stream-category-whitespace]
                 [(scheme-derived-token-has-tag? token 'keyword) stream-category-keyword]
                 [(scheme-derived-token-has-tag? token 'literal) stream-category-literal]
                 [(scheme-derived-token-has-tag? token 'delimiter) stream-category-delimiter]
                 [else stream-category-identifier]))
         (cond [(eq? category stream-category-unknown)
                (case (scheme-config-errors config)
                  [(emit-unknown)
                   (wrap-token-with-pos (make-stream-token category (scheme-derived-token-text token))
                                        (scheme-derived-token-start token)
                                        (scheme-derived-token-end token)
                                        (scheme-config-source-positions config))]
                  [(raise)
                   (define start (scheme-derived-token-start token))
                   (define end (scheme-derived-token-end token))
                   (raise-read-error "malformed Scheme input" #f
                                     (position-line start) (position-col start)
                                     (position-offset start)
                                     (- (position-offset end) (position-offset start)))])]
               [(and (eq? (scheme-config-trivia config) 'skip)
                     (member category '(comment whitespace))) #f]
               [else
                (wrap-token-with-pos (make-stream-token category (scheme-derived-token-text token))
                                     (scheme-derived-token-start token)
                                     (scheme-derived-token-end token)
                                     (scheme-config-source-positions config))])]))
