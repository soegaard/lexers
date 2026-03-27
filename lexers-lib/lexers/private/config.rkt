#lang racket/base

;;;
;;; Lexer Configuration
;;;
;;
;; Shared profile and option handling for lexer configuration.

;; css-profile-defaults  : immutable-hash?
;;   Default settings for named CSS lexer profiles.
;; css-config?           : any/c -> boolean?
;;   Recognize CSS lexer configuration values.
;; css-config-profile    : css-config? -> symbol?
;;   Extract the configured profile name.
;; css-config-trivia     : css-config? -> symbol?
;;   Extract the configured trivia policy.
;; css-config-source-positions : css-config? -> boolean?
;;   Extract the configured source-position setting.
;; css-config-errors     : css-config? -> symbol?
;;   Extract the configured error policy.
;; make-css-config       : keyword-arguments -> css-config?
;;   Resolve profile defaults and explicit overrides into one config.

(provide css-profile-defaults
         css-config?
         css-config-profile
         css-config-trivia
         css-config-source-positions
         css-config-errors
         make-css-config)

;; A resolved configuration for the public CSS lexer.
(struct css-config (profile trivia source-positions errors) #:transparent)

;; Profile defaults for the CSS lexer.
(define css-profile-defaults
  (hash 'coloring (hash 'trivia           'keep
                        'source-positions #t
                        'errors           'emit-unknown)
        'compiler (hash 'trivia           'skip
                        'source-positions #t
                        'errors           'raise)))

;; make-css-config : keyword-arguments -> css-config?
;;   Resolve profile defaults and explicit overrides into one config.
(define (make-css-config #:profile          [profile 'coloring]
                         #:trivia           [trivia 'profile-default]
                         #:source-positions [source-positions 'profile-default])
  (define defaults
    (hash-ref css-profile-defaults
              profile
              (lambda ()
                (raise-arguments-error 'make-css-config
                                       "unknown CSS lexer profile"
                                       "profile" profile))))
  (define resolved-trivia
    (case trivia
      [(profile-default) (hash-ref defaults 'trivia)]
      [else              trivia]))
  (define resolved-source-positions
    (case source-positions
      [(profile-default) (hash-ref defaults 'source-positions)]
      [else              source-positions]))
  (define resolved-errors (hash-ref defaults 'errors))
  (css-config profile
              resolved-trivia
              resolved-source-positions
              resolved-errors))
