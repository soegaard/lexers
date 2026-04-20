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
;; c-profile-defaults : immutable-hash?
;;   Default settings for named C lexer profiles.
;; csv-profile-defaults : immutable-hash?
;;   Default settings for named CSV lexer profiles.
;; tsv-profile-defaults : immutable-hash?
;;   Default settings for named TSV lexer profiles.
;; yaml-profile-defaults : immutable-hash?
;;   Default settings for named YAML lexer profiles.
;; json-profile-defaults : immutable-hash?
;;   Default settings for named JSON lexer profiles.
;; swift-profile-defaults : immutable-hash?
;;   Default settings for named Swift lexer profiles.
;; python-profile-defaults : immutable-hash?
;;   Default settings for named Python lexer profiles.
;; rhombus-profile-defaults : immutable-hash?
;;   Default settings for named Rhombus lexer profiles.
;; wat-profile-defaults : immutable-hash?
;;   Default settings for named WAT lexer profiles.
;; shell-profile-defaults : immutable-hash?
;;   Default settings for named shell lexer profiles.
;; racket-profile-defaults : immutable-hash?
;;   Default settings for named Racket lexer profiles.
;; markdown-profile-defaults : immutable-hash?
;;   Default settings for named Markdown lexer profiles.
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
;; c-config?             : any/c -> boolean?
;;   Recognize C lexer configuration values.
;; c-config-profile      : c-config? -> symbol?
;;   Extract the configured profile name.
;; c-config-trivia       : c-config? -> symbol?
;;   Extract the configured trivia policy.
;; c-config-source-positions : c-config? -> boolean?
;;   Extract the configured source-position setting.
;; c-config-errors       : c-config? -> symbol?
;;   Extract the configured error policy.
;; make-c-config         : keyword-arguments -> c-config?
;;   Resolve profile defaults and explicit overrides into one config.
;; csv-config?           : any/c -> boolean?
;;   Recognize CSV lexer configuration values.
;; csv-config-profile    : csv-config? -> symbol?
;;   Extract the configured profile name.
;; csv-config-trivia     : csv-config? -> symbol?
;;   Extract the configured trivia policy.
;; csv-config-source-positions : csv-config? -> boolean?
;;   Extract the configured source-position setting.
;; csv-config-errors     : csv-config? -> symbol?
;;   Extract the configured error policy.
;; make-csv-config       : keyword-arguments -> csv-config?
;;   Resolve profile defaults and explicit overrides into one config.
;; tsv-config?           : any/c -> boolean?
;;   Recognize TSV lexer configuration values.
;; tsv-config-profile    : tsv-config? -> symbol?
;;   Extract the configured profile name.
;; tsv-config-trivia     : tsv-config? -> symbol?
;;   Extract the configured trivia policy.
;; tsv-config-source-positions : tsv-config? -> boolean?
;;   Extract the configured source-position setting.
;; tsv-config-errors     : tsv-config? -> symbol?
;;   Extract the configured error policy.
;; make-tsv-config       : keyword-arguments -> tsv-config?
;;   Resolve profile defaults and explicit overrides into one config.
;; yaml-config?          : any/c -> boolean?
;;   Recognize YAML lexer configuration values.
;; yaml-config-profile   : yaml-config? -> symbol?
;;   Extract the configured profile name.
;; yaml-config-trivia    : yaml-config? -> symbol?
;;   Extract the configured trivia policy.
;; yaml-config-source-positions : yaml-config? -> boolean?
;;   Extract the configured source-position setting.
;; yaml-config-errors    : yaml-config? -> symbol?
;;   Extract the configured error policy.
;; make-yaml-config      : keyword-arguments -> yaml-config?
;;   Resolve profile defaults and explicit overrides into one config.
;; json-config?          : any/c -> boolean?
;;   Recognize JSON lexer configuration values.
;; json-config-profile   : json-config? -> symbol?
;;   Extract the configured profile name.
;; json-config-trivia    : json-config? -> symbol?
;;   Extract the configured trivia policy.
;; json-config-source-positions : json-config? -> boolean?
;;   Extract the configured source-position setting.
;; json-config-errors    : json-config? -> symbol?
;;   Extract the configured error policy.
;; make-json-config      : keyword-arguments -> json-config?
;;   Resolve profile defaults and explicit overrides into one config.
;; swift-config?         : any/c -> boolean?
;;   Recognize Swift lexer configuration values.
;; swift-config-profile  : swift-config? -> symbol?
;;   Extract the configured profile name.
;; swift-config-trivia   : swift-config? -> symbol?
;;   Extract the configured trivia policy.
;; swift-config-source-positions : swift-config? -> boolean?
;;   Extract the configured source-position setting.
;; swift-config-errors   : swift-config? -> symbol?
;;   Extract the configured error policy.
;; make-swift-config     : keyword-arguments -> swift-config?
;;   Resolve profile defaults and explicit overrides into one config.
;; python-config?        : any/c -> boolean?
;;   Recognize Python lexer configuration values.
;; python-config-profile : python-config? -> symbol?
;;   Extract the configured profile name.
;; python-config-trivia  : python-config? -> symbol?
;;   Extract the configured trivia policy.
;; python-config-source-positions : python-config? -> boolean?
;;   Extract the configured source-position setting.
;; python-config-errors  : python-config? -> symbol?
;;   Extract the configured error policy.
;; make-python-config    : keyword-arguments -> python-config?
;;   Resolve profile defaults and explicit overrides into one config.
;; rhombus-config?       : any/c -> boolean?
;;   Recognize Rhombus lexer configuration values.
;; rhombus-config-profile : rhombus-config? -> symbol?
;;   Extract the configured profile name.
;; rhombus-config-trivia : rhombus-config? -> symbol?
;;   Extract the configured trivia policy.
;; rhombus-config-source-positions : rhombus-config? -> boolean?
;;   Extract the configured source-position setting.
;; rhombus-config-errors : rhombus-config? -> symbol?
;;   Extract the configured error policy.
;; make-rhombus-config   : keyword-arguments -> rhombus-config?
;;   Resolve profile defaults and explicit overrides into one config.
;; wat-config?           : any/c -> boolean?
;;   Recognize WAT lexer configuration values.
;; wat-config-profile    : wat-config? -> symbol?
;;   Extract the configured profile name.
;; wat-config-trivia     : wat-config? -> symbol?
;;   Extract the configured trivia policy.
;; wat-config-source-positions : wat-config? -> boolean?
;;   Extract the configured source-position setting.
;; wat-config-errors     : wat-config? -> symbol?
;;   Extract the configured error policy.
;; make-wat-config       : keyword-arguments -> wat-config?
;;   Resolve profile defaults and explicit overrides into one config.
;; shell-config?         : any/c -> boolean?
;;   Recognize shell lexer configuration values.
;; shell-config-profile  : shell-config? -> symbol?
;;   Extract the configured profile name.
;; shell-config-trivia   : shell-config? -> symbol?
;;   Extract the configured trivia policy.
;; shell-config-source-positions : shell-config? -> boolean?
;;   Extract the configured source-position setting.
;; shell-config-shell    : shell-config? -> symbol?
;;   Extract the configured shell dialect.
;; shell-config-errors   : shell-config? -> symbol?
;;   Extract the configured error policy.
;; make-shell-config     : keyword-arguments -> shell-config?
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
;; markdown-config?      : any/c -> boolean?
;;   Recognize Markdown lexer configuration values.
;; markdown-config-profile : markdown-config? -> symbol?
;;   Extract the configured profile name.
;; markdown-config-trivia : markdown-config? -> symbol?
;;   Extract the configured trivia policy.
;; markdown-config-source-positions : markdown-config? -> boolean?
;;   Extract the configured source-position setting.
;; markdown-config-errors : markdown-config? -> symbol?
;;   Extract the configured error policy.
;; make-markdown-config  : keyword-arguments -> markdown-config?
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
         c-profile-defaults
         cpp-profile-defaults
         objc-profile-defaults
         csv-profile-defaults
         tsv-profile-defaults
         yaml-profile-defaults
         json-profile-defaults
         swift-profile-defaults
         python-profile-defaults
         rhombus-profile-defaults
         wat-profile-defaults
         shell-profile-defaults
         racket-profile-defaults
         markdown-profile-defaults
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
         c-config?
         c-config-profile
         c-config-trivia
         c-config-source-positions
         c-config-errors
         make-c-config
         cpp-config?
         cpp-config-profile
         cpp-config-trivia
         cpp-config-source-positions
         cpp-config-errors
         make-cpp-config
         objc-config?
         objc-config-profile
         objc-config-trivia
         objc-config-source-positions
         objc-config-errors
         make-objc-config
         csv-config?
         csv-config-profile
         csv-config-trivia
         csv-config-source-positions
         csv-config-errors
         make-csv-config
         tsv-config?
         tsv-config-profile
         tsv-config-trivia
         tsv-config-source-positions
         tsv-config-errors
         make-tsv-config
         yaml-config?
         yaml-config-profile
         yaml-config-trivia
         yaml-config-source-positions
         yaml-config-errors
         make-yaml-config
         json-config?
         json-config-profile
         json-config-trivia
         json-config-source-positions
         json-config-errors
         make-json-config
         swift-config?
         swift-config-profile
         swift-config-trivia
         swift-config-source-positions
         swift-config-errors
         make-swift-config
         python-config?
         python-config-profile
         python-config-trivia
         python-config-source-positions
         python-config-errors
         make-python-config
         rhombus-config?
         rhombus-config-profile
         rhombus-config-trivia
         rhombus-config-source-positions
         rhombus-config-errors
         make-rhombus-config
         wat-config?
         wat-config-profile
         wat-config-trivia
         wat-config-source-positions
         wat-config-errors
         make-wat-config
         shell-config?
         shell-config-profile
         shell-config-trivia
         shell-config-source-positions
         shell-config-shell
         shell-config-errors
         make-shell-config
         racket-config?
         racket-config-profile
         racket-config-trivia
         racket-config-source-positions
         racket-config-errors
         make-racket-config
         markdown-config?
         markdown-config-profile
         markdown-config-trivia
         markdown-config-source-positions
         markdown-config-errors
         make-markdown-config
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

;; A resolved configuration for the public C lexer.
(struct c-config (profile trivia source-positions errors) #:transparent)

;; A resolved configuration for the public C++ lexer.
(struct cpp-config (profile trivia source-positions errors) #:transparent)

;; A resolved configuration for the public Objective-C lexer.
(struct objc-config (profile trivia source-positions errors) #:transparent)

;; A resolved configuration for the public CSV lexer.
(struct csv-config (profile trivia source-positions errors) #:transparent)

;; A resolved configuration for the public TSV lexer.
(struct tsv-config (profile trivia source-positions errors) #:transparent)

;; A resolved configuration for the public YAML lexer.
(struct yaml-config (profile trivia source-positions errors) #:transparent)

;; A resolved configuration for the public JSON lexer.
(struct json-config (profile trivia source-positions errors) #:transparent)

;; A resolved configuration for the public Swift lexer.
(struct swift-config (profile trivia source-positions errors) #:transparent)

;; A resolved configuration for the public Python lexer.
(struct python-config (profile trivia source-positions errors) #:transparent)

;; A resolved configuration for the public Rhombus lexer.
(struct rhombus-config (profile trivia source-positions errors) #:transparent)

;; A resolved configuration for the public WAT lexer.
(struct wat-config (profile trivia source-positions errors) #:transparent)

;; A resolved configuration for the public shell lexer.
(struct shell-config (profile trivia source-positions shell errors) #:transparent)

;; A resolved configuration for the public Racket lexer.
(struct racket-config (profile trivia source-positions errors) #:transparent)

;; A resolved configuration for the public Markdown lexer.
(struct markdown-config (profile trivia source-positions errors) #:transparent)

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

;; Profile defaults for the C lexer.
(define c-profile-defaults
  (hash 'coloring (hash 'trivia           'keep
                        'source-positions #t
                        'errors           'emit-unknown)
        'compiler (hash 'trivia           'skip
                        'source-positions #t
                        'errors           'raise)))

;; Profile defaults for the C++ lexer.
(define cpp-profile-defaults
  (hash 'coloring (hash 'trivia           'keep
                        'source-positions #t
                        'errors           'emit-unknown)
        'compiler (hash 'trivia           'skip
                        'source-positions #t
                        'errors           'raise)))

;; Profile defaults for the Objective-C lexer.
(define objc-profile-defaults
  (hash 'coloring (hash 'trivia           'keep
                        'source-positions #t
                        'errors           'emit-unknown)
        'compiler (hash 'trivia           'skip
                        'source-positions #t
                        'errors           'raise)))

;; Profile defaults for the CSV lexer.
(define csv-profile-defaults
  (hash 'coloring (hash 'trivia           'keep
                        'source-positions #t
                        'errors           'emit-unknown)
        'compiler (hash 'trivia           'skip
                        'source-positions #t
                        'errors           'raise)))

;; Profile defaults for the TSV lexer.
(define tsv-profile-defaults
  (hash 'coloring (hash 'trivia           'keep
                        'source-positions #t
                        'errors           'emit-unknown)
        'compiler (hash 'trivia           'skip
                        'source-positions #t
                        'errors           'raise)))

;; Profile defaults for the YAML lexer.
(define yaml-profile-defaults
  (hash 'coloring (hash 'trivia           'keep
                        'source-positions #t
                        'errors           'emit-unknown)
        'compiler (hash 'trivia           'skip
                        'source-positions #t
                        'errors           'raise)))

;; Profile defaults for the JSON lexer.
(define json-profile-defaults
  (hash 'coloring (hash 'trivia           'keep
                        'source-positions #t
                        'errors           'emit-unknown)
        'compiler (hash 'trivia           'skip
                        'source-positions #t
                        'errors           'raise)))

;; Profile defaults for the Swift lexer.
(define swift-profile-defaults
  (hash 'coloring (hash 'trivia           'keep
                        'source-positions #t
                        'errors           'emit-unknown)
        'compiler (hash 'trivia           'skip
                        'source-positions #t
                        'errors           'raise)))

;; Profile defaults for the Python lexer.
(define python-profile-defaults
  (hash 'coloring (hash 'trivia           'keep
                        'source-positions #t
                        'errors           'emit-unknown)
        'compiler (hash 'trivia           'skip
                        'source-positions #t
                        'errors           'raise)))

;; Profile defaults for the Rhombus lexer.
(define rhombus-profile-defaults
  (hash 'coloring (hash 'trivia           'keep
                        'source-positions #t
                        'errors           'emit-unknown)
        'compiler (hash 'trivia           'skip
                        'source-positions #t
                        'errors           'raise)))

;; Profile defaults for the WAT lexer.
(define wat-profile-defaults
  (hash 'coloring (hash 'trivia           'keep
                        'source-positions #t
                        'errors           'emit-unknown)
        'compiler (hash 'trivia           'skip
                        'source-positions #t
                        'errors           'raise)))

;; Profile defaults for the shell lexer.
(define shell-profile-defaults
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

;; Profile defaults for the Markdown lexer.
(define markdown-profile-defaults
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

;; make-c-config : keyword-arguments -> c-config?
;;   Resolve profile defaults and explicit overrides into one config.
(define (make-c-config #:profile          [profile 'coloring]
                       #:trivia           [trivia 'profile-default]
                       #:source-positions [source-positions 'profile-default])
  (define defaults
    (hash-ref c-profile-defaults
              profile
              (lambda ()
                (raise-arguments-error 'make-c-config
                                       "unknown C lexer profile"
                                       "profile" profile))))
  (define resolved-trivia
    (case trivia
      [(profile-default) (hash-ref defaults 'trivia)]
      [else              trivia]))
  (define resolved-source-positions
    (case source-positions
      [(profile-default) (hash-ref defaults 'source-positions)]
      [else              source-positions]))
  (define resolved-errors
    (hash-ref defaults 'errors))
  (c-config profile
            resolved-trivia
            resolved-source-positions
            resolved-errors))

;; make-cpp-config : keyword-arguments -> cpp-config?
;;   Resolve profile defaults and explicit overrides into one config.
(define (make-cpp-config #:profile          [profile 'coloring]
                         #:trivia           [trivia 'profile-default]
                         #:source-positions [source-positions 'profile-default])
  (define defaults
    (hash-ref cpp-profile-defaults
              profile
              (lambda ()
                (raise-arguments-error 'make-cpp-config
                                       "unknown C++ lexer profile"
                                       "profile" profile))))
  (define resolved-trivia
    (case trivia
      [(profile-default) (hash-ref defaults 'trivia)]
      [else              trivia]))
  (define resolved-source-positions
    (case source-positions
      [(profile-default) (hash-ref defaults 'source-positions)]
      [else              source-positions]))
  (define resolved-errors
    (hash-ref defaults 'errors))
  (cpp-config profile
              resolved-trivia
              resolved-source-positions
              resolved-errors))

;; make-objc-config : keyword-arguments -> objc-config?
;;   Resolve profile defaults and explicit overrides into one config.
(define (make-objc-config #:profile          [profile 'coloring]
                          #:trivia           [trivia 'profile-default]
                          #:source-positions [source-positions 'profile-default])
  (define defaults
    (hash-ref objc-profile-defaults
              profile
              (lambda ()
                (raise-arguments-error 'make-objc-config
                                       "unknown Objective-C lexer profile"
                                       "profile" profile))))
  (define resolved-trivia
    (case trivia
      [(profile-default) (hash-ref defaults 'trivia)]
      [else              trivia]))
  (define resolved-source-positions
    (case source-positions
      [(profile-default) (hash-ref defaults 'source-positions)]
      [else              source-positions]))
  (define resolved-errors
    (hash-ref defaults 'errors))
  (objc-config profile
               resolved-trivia
               resolved-source-positions
               resolved-errors))

;; make-csv-config : keyword-arguments -> csv-config?
;;   Resolve profile defaults and explicit overrides into one config.
(define (make-csv-config #:profile          [profile 'coloring]
                         #:trivia           [trivia 'profile-default]
                         #:source-positions [source-positions 'profile-default])
  (define defaults
    (hash-ref csv-profile-defaults
              profile
              (lambda ()
                (raise-arguments-error 'make-csv-config
                                       "unknown CSV lexer profile"
                                       "profile" profile))))
  (define resolved-trivia
    (case trivia
      [(profile-default) (hash-ref defaults 'trivia)]
      [else              trivia]))
  (define resolved-source-positions
    (case source-positions
      [(profile-default) (hash-ref defaults 'source-positions)]
      [else              source-positions]))
  (define resolved-errors
    (hash-ref defaults 'errors))
  (csv-config profile
              resolved-trivia
              resolved-source-positions
              resolved-errors))

;; make-tsv-config : keyword-arguments -> tsv-config?
;;   Resolve profile defaults and explicit overrides into one config.
(define (make-tsv-config #:profile          [profile 'coloring]
                         #:trivia           [trivia 'profile-default]
                         #:source-positions [source-positions 'profile-default])
  (define defaults
    (hash-ref tsv-profile-defaults
              profile
              (lambda ()
                (raise-arguments-error 'make-tsv-config
                                       "unknown TSV lexer profile"
                                       "profile" profile))))
  (define resolved-trivia
    (case trivia
      [(profile-default) (hash-ref defaults 'trivia)]
      [else              trivia]))
  (define resolved-source-positions
    (case source-positions
      [(profile-default) (hash-ref defaults 'source-positions)]
      [else              source-positions]))
  (define resolved-errors
    (hash-ref defaults 'errors))
  (tsv-config profile
              resolved-trivia
              resolved-source-positions
              resolved-errors))

;; make-yaml-config : keyword-arguments -> yaml-config?
;;   Resolve profile defaults and explicit overrides into one config.
(define (make-yaml-config #:profile          [profile 'coloring]
                          #:trivia           [trivia 'profile-default]
                          #:source-positions [source-positions 'profile-default])
  (define defaults
    (hash-ref yaml-profile-defaults
              profile
              (lambda ()
                (raise-arguments-error 'make-yaml-config
                                       "unknown YAML lexer profile"
                                       "profile" profile))))
  (define resolved-trivia
    (case trivia
      [(profile-default) (hash-ref defaults 'trivia)]
      [else              trivia]))
  (define resolved-source-positions
    (case source-positions
      [(profile-default) (hash-ref defaults 'source-positions)]
      [else              source-positions]))
  (define resolved-errors
    (hash-ref defaults 'errors))
  (yaml-config profile
               resolved-trivia
               resolved-source-positions
               resolved-errors))

;; make-json-config : keyword-arguments -> json-config?
;;   Resolve profile defaults and explicit overrides into one config.
(define (make-json-config #:profile          [profile 'coloring]
                          #:trivia           [trivia 'profile-default]
                          #:source-positions [source-positions 'profile-default])
  (define defaults
    (hash-ref json-profile-defaults
              profile
              (lambda ()
                (raise-arguments-error 'make-json-config
                                       "unknown JSON lexer profile"
                                       "profile" profile))))
  (define resolved-trivia
    (case trivia
      [(profile-default) (hash-ref defaults 'trivia)]
      [else              trivia]))
  (define resolved-source-positions
    (case source-positions
      [(profile-default) (hash-ref defaults 'source-positions)]
      [else              source-positions]))
  (define resolved-errors
    (hash-ref defaults 'errors))
  (json-config profile
               resolved-trivia
               resolved-source-positions
               resolved-errors))

;; make-swift-config : keyword-arguments -> swift-config?
;;   Resolve profile defaults and explicit overrides into one config.
(define (make-swift-config #:profile          [profile 'coloring]
                           #:trivia           [trivia 'profile-default]
                           #:source-positions [source-positions 'profile-default])
  (define defaults
    (hash-ref swift-profile-defaults
              profile
              (lambda ()
                (raise-arguments-error 'make-swift-config
                                       "unknown Swift lexer profile"
                                       "profile" profile))))
  (define resolved-trivia
    (case trivia
      [(profile-default) (hash-ref defaults 'trivia)]
      [else              trivia]))
  (define resolved-source-positions
    (case source-positions
      [(profile-default) (hash-ref defaults 'source-positions)]
      [else              source-positions]))
  (define resolved-errors
    (hash-ref defaults 'errors))
  (swift-config profile
                resolved-trivia
                resolved-source-positions
                resolved-errors))

;; make-python-config : keyword-arguments -> python-config?
;;   Resolve profile defaults and explicit overrides into one config.
(define (make-python-config #:profile          [profile 'coloring]
                            #:trivia           [trivia 'profile-default]
                            #:source-positions [source-positions 'profile-default])
  (define defaults
    (hash-ref python-profile-defaults
              profile
              (lambda ()
                (raise-arguments-error 'make-python-config
                                       "unknown Python lexer profile"
                                       "profile" profile))))
  (define resolved-trivia
    (case trivia
      [(profile-default) (hash-ref defaults 'trivia)]
      [else              trivia]))
  (define resolved-source-positions
    (case source-positions
      [(profile-default) (hash-ref defaults 'source-positions)]
      [else              source-positions]))
  (define resolved-errors
    (hash-ref defaults 'errors))
  (python-config profile
                 resolved-trivia
                 resolved-source-positions
                 resolved-errors))

;; make-rhombus-config : keyword-arguments -> rhombus-config?
;;   Resolve profile defaults and explicit overrides into one config.
(define (make-rhombus-config #:profile          [profile 'coloring]
                             #:trivia           [trivia 'profile-default]
                             #:source-positions [source-positions 'profile-default])
  (define defaults
    (hash-ref rhombus-profile-defaults
              profile
              (lambda ()
                (raise-arguments-error 'make-rhombus-config
                                       "unknown Rhombus lexer profile"
                                       "profile" profile))))
  (define resolved-trivia
    (case trivia
      [(profile-default) (hash-ref defaults 'trivia)]
      [else              trivia]))
  (define resolved-source-positions
    (case source-positions
      [(profile-default) (hash-ref defaults 'source-positions)]
      [else              source-positions]))
  (define resolved-errors
    (hash-ref defaults 'errors))
  (rhombus-config profile
                  resolved-trivia
                  resolved-source-positions
                  resolved-errors))

;; make-wat-config : keyword-arguments -> wat-config?
;;   Resolve profile defaults and explicit overrides into one config.
(define (make-wat-config #:profile          [profile 'coloring]
                         #:trivia           [trivia 'profile-default]
                         #:source-positions [source-positions 'profile-default])
  (define defaults
    (hash-ref wat-profile-defaults
              profile
              (lambda ()
                (raise-arguments-error 'make-wat-config
                                       "unknown WAT lexer profile"
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
  (wat-config profile
              resolved-trivia
              resolved-source-positions
              resolved-errors))

;; normalize-shell-dialect : symbol? -> symbol?
;;   Normalize accepted shell dialect names.
(define (normalize-shell-dialect who shell)
  (case shell
    [(bash zsh powershell) shell]
    [(pwsh)                'powershell]
    [else
     (raise-arguments-error who
                            "unknown shell dialect"
                            "shell" shell)]))

;; make-shell-config : keyword-arguments -> shell-config?
;;   Resolve profile defaults and explicit overrides into one config.
(define (make-shell-config #:profile          [profile 'coloring]
                           #:trivia           [trivia 'profile-default]
                           #:source-positions [source-positions 'profile-default]
                           #:shell            [shell 'bash])
  (define defaults
    (hash-ref shell-profile-defaults
              profile
              (lambda ()
                (raise-arguments-error 'make-shell-config
                                       "unknown shell lexer profile"
                                       "profile" profile))))
  (define resolved-trivia
    (case trivia
      [(profile-default) (hash-ref defaults 'trivia)]
      [else              trivia]))
  (define resolved-source-positions
    (case source-positions
      [(profile-default) (hash-ref defaults 'source-positions)]
      [else              source-positions]))
  (define resolved-shell
    (normalize-shell-dialect 'make-shell-config shell))
  (define resolved-errors
    (hash-ref defaults 'errors))
  (shell-config profile
                resolved-trivia
                resolved-source-positions
                resolved-shell
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

;; make-markdown-config : keyword-arguments -> markdown-config?
;;   Resolve profile defaults and explicit overrides into one config.
(define (make-markdown-config #:profile          [profile 'coloring]
                              #:trivia           [trivia 'profile-default]
                              #:source-positions [source-positions 'profile-default])
  (define defaults
    (hash-ref markdown-profile-defaults
              profile
              (lambda ()
                (raise-arguments-error 'make-markdown-config
                                       "unknown Markdown lexer profile"
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
  (markdown-config profile
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
