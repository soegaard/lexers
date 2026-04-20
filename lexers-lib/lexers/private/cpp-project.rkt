#lang racket/base

;;;
;;; C++ Projection
;;;
;;
;; Project derived C++ tokens into the reusable stream model.

;; project-cpp-derived-token : (or/c cpp-derived-token? 'eof) cpp-config? -> token-like?
;;   Convert a derived C++ token into a reusable-stream token-like value.

(provide project-cpp-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "cpp-derived.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt")

;; skip-trivia? : cpp-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (cpp-config-trivia config) 'skip))

;; derived->stream-category : cpp-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(cpp-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(cpp-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(cpp-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(cpp-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(cpp-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(cpp-derived-token-has-tag? derived-token 'operator)
     stream-category-operator]
    [(cpp-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [else
     stream-category-identifier]))

;; raw-eof->token : cpp-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (cpp-config-source-positions config)))

;; malformed-token->result : cpp-derived-token? cpp-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (cpp-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (cpp-derived-token-text derived-token))
      (cpp-derived-token-start derived-token)
      (cpp-derived-token-end derived-token)
      (cpp-config-source-positions config))]
    [(raise)
     (define start-pos
       (cpp-derived-token-start derived-token))
     (define end-pos
       (cpp-derived-token-end derived-token))
     (raise-read-error "unknown C++ input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-cpp-derived-token
            "unsupported C++ error policy: ~a"
            (cpp-config-errors config))]))

;; visible-derived-token? : cpp-derived-token? cpp-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : cpp-derived-token? cpp-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (cpp-derived-token-text derived-token))
   (cpp-derived-token-start derived-token)
   (cpp-derived-token-end derived-token)
   (cpp-config-source-positions config)))

;; project-cpp-derived-token : (or/c cpp-derived-token? 'eof) cpp-config? -> token-like?
;;   Convert a derived C++ token into the reusable stream model.
(define (project-cpp-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(cpp-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
