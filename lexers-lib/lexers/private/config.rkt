#lang racket/base

;;;
;;; Lexer Configuration
;;;
;;
;; Shared profile and option handling for lexer configuration.

;; css-profile-defaults  : immutable-hash?
;;   Default settings for named CSS lexer profiles.
;; html-profile-defaults : immutable-hash?
;;   Default settings for named HTML lexer profiles.
;; racket-profile-defaults : immutable-hash?
;;   Default settings for named Racket lexer profiles.
;; scribble-profile-defaults : immutable-hash?
;;   Default settings for named Scribble lexer profiles.
;; javascript-profile-defaults : immutable-hash?
;;   Default settings for named JavaScript lexer profiles.
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
;; html-config?          : any/c -> boolean?
;;   Recognize HTML lexer configuration values.
;; html-config-profile   : html-config? -> symbol?
;;   Extract the configured profile name.
;; html-config-trivia    : html-config? -> symbol?
;;   Extract the configured trivia policy.
;; html-config-source-positions : html-config? -> boolean?
;;   Extract the configured source-position setting.
;; html-config-errors    : html-config? -> symbol?
;;   Extract the configured error policy.
;; make-html-config      : keyword-arguments -> html-config?
;;   Resolve profile defaults and explicit overrides into one config.
;; racket-config?        : any/c -> boolean?
;;   Recognize Racket lexer configuration values.
;; racket-config-profile : racket-config? -> symbol?
;;   Extract the configured profile name.
;; racket-config-trivia  : racket-config? -> symbol?
;;   Extract the configured trivia policy.
;; racket-config-source-positions : racket-config? -> boolean?
;;   Extract the configured source-position setting.
;; racket-config-errors  : racket-config? -> symbol?
;;   Extract the configured error policy.
;; make-racket-config    : keyword-arguments -> racket-config?
;;   Resolve profile defaults and explicit overrides into one config.
;; scribble-config?      : any/c -> boolean?
;;   Recognize Scribble lexer configuration values.
;; scribble-config-profile : scribble-config? -> symbol?
;;   Extract the configured profile name.
;; scribble-config-trivia : scribble-config? -> symbol?
;;   Extract the configured trivia policy.
;; scribble-config-source-positions : scribble-config? -> boolean?
;;   Extract the configured source-position setting.
;; scribble-config-errors : scribble-config? -> symbol?
;;   Extract the configured error policy.
;; make-scribble-config  : keyword-arguments -> scribble-config?
;;   Resolve profile defaults and explicit overrides into one config.
;; javascript-config?    : any/c -> boolean?
;;   Recognize JavaScript lexer configuration values.
;; javascript-config-profile : javascript-config? -> symbol?
;;   Extract the configured profile name.
;; javascript-config-trivia : javascript-config? -> symbol?
;;   Extract the configured trivia policy.
;; javascript-config-source-positions : javascript-config? -> boolean?
;;   Extract the configured source-position setting.
;; javascript-config-jsx? : javascript-config? -> boolean?
;;   Extract whether JSX support is enabled for the JavaScript lexer.
;; javascript-config-errors : javascript-config? -> symbol?
;;   Extract the configured error policy.
;; make-javascript-config : keyword-arguments -> javascript-config?
;;   Resolve profile defaults and explicit overrides into one config.

(provide css-profile-defaults
         html-profile-defaults
         racket-profile-defaults
         scribble-profile-defaults
         javascript-profile-defaults
         css-config?
         css-config-profile
         css-config-trivia
         css-config-source-positions
         css-config-errors
         make-css-config
         html-config?
         html-config-profile
         html-config-trivia
         html-config-source-positions
         html-config-errors
         make-html-config
         racket-config?
         racket-config-profile
         racket-config-trivia
         racket-config-source-positions
         racket-config-errors
         make-racket-config
         scribble-config?
         scribble-config-profile
         scribble-config-trivia
         scribble-config-source-positions
         scribble-config-errors
         make-scribble-config
         javascript-config?
         javascript-config-profile
         javascript-config-trivia
         javascript-config-source-positions
         javascript-config-jsx?
         javascript-config-errors
         make-javascript-config)

;; A resolved configuration for the public CSS lexer.
(struct css-config (profile trivia source-positions errors) #:transparent)

;; A resolved configuration for the public HTML lexer.
(struct html-config (profile trivia source-positions errors) #:transparent)

;; A resolved configuration for the public Racket lexer.
(struct racket-config (profile trivia source-positions errors) #:transparent)

;; A resolved configuration for the public Scribble lexer.
(struct scribble-config (profile trivia source-positions errors) #:transparent)

;; A resolved configuration for the public JavaScript lexer.
(struct javascript-config (profile trivia source-positions jsx? errors) #:transparent)

;; Profile defaults for the CSS lexer.
(define css-profile-defaults
  (hash 'coloring (hash 'trivia           'keep
                        'source-positions #t
                        'errors           'emit-unknown)
        'compiler (hash 'trivia           'skip
                        'source-positions #t
                        'errors           'raise)))

;; Profile defaults for the HTML lexer.
(define html-profile-defaults
  (hash 'coloring (hash 'trivia           'keep
                        'source-positions #t
                        'errors           'emit-unknown)
        'compiler (hash 'trivia           'skip
                        'source-positions #t
                        'errors           'raise)))

;; Profile defaults for the Racket lexer.
(define racket-profile-defaults
  (hash 'coloring (hash 'trivia           'keep
                        'source-positions #t
                        'errors           'emit-unknown)
        'compiler (hash 'trivia           'skip
                        'source-positions #t
                        'errors           'raise)))

;; Profile defaults for the Scribble lexer.
(define scribble-profile-defaults
  (hash 'coloring (hash 'trivia           'keep
                        'source-positions #t
                        'errors           'emit-unknown)
        'compiler (hash 'trivia           'skip
                        'source-positions #t
                        'errors           'raise)))

;; Profile defaults for the JavaScript lexer.
(define javascript-profile-defaults
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

;; make-html-config : keyword-arguments -> html-config?
;;   Resolve profile defaults and explicit overrides into one config.
(define (make-html-config #:profile          [profile 'coloring]
                          #:trivia           [trivia 'profile-default]
                          #:source-positions [source-positions 'profile-default])
  (define defaults
    (hash-ref html-profile-defaults
              profile
              (lambda ()
                (raise-arguments-error 'make-html-config
                                       "unknown HTML lexer profile"
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
  (html-config profile
               resolved-trivia
               resolved-source-positions
               resolved-errors))

;; make-racket-config : keyword-arguments -> racket-config?
;;   Resolve profile defaults and explicit overrides into one config.
(define (make-racket-config #:profile          [profile 'coloring]
                            #:trivia           [trivia 'profile-default]
                            #:source-positions [source-positions 'profile-default])
  (define defaults
    (hash-ref racket-profile-defaults
              profile
              (lambda ()
                (raise-arguments-error 'make-racket-config
                                       "unknown Racket lexer profile"
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
  (racket-config profile
                 resolved-trivia
                 resolved-source-positions
                 resolved-errors))

;; make-scribble-config : keyword-arguments -> scribble-config?
;;   Resolve profile defaults and explicit overrides into one config.
(define (make-scribble-config #:profile          [profile 'coloring]
                              #:trivia           [trivia 'profile-default]
                              #:source-positions [source-positions 'profile-default])
  (define defaults
    (hash-ref scribble-profile-defaults
              profile
              (lambda ()
                (raise-arguments-error 'make-scribble-config
                                       "unknown Scribble lexer profile"
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
  (scribble-config profile
                   resolved-trivia
                   resolved-source-positions
                   resolved-errors))

;; make-javascript-config : keyword-arguments -> javascript-config?
;;   Resolve profile defaults and explicit overrides into one config.
(define (make-javascript-config #:profile          [profile 'coloring]
                                #:trivia           [trivia 'profile-default]
                                #:source-positions [source-positions 'profile-default]
                                #:jsx?             [jsx? #f])
  (define defaults
    (hash-ref javascript-profile-defaults
              profile
              (lambda ()
                (raise-arguments-error 'make-javascript-config
                                       "unknown JavaScript lexer profile"
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
  (javascript-config profile
                     resolved-trivia
                     resolved-source-positions
                     jsx?
                     resolved-errors))
