#lang racket/base

;;;
;;; CSS Derived Classifications
;;;
;;
;; Derived CSS-specific classifications layered on top of raw CSS tokens.

;; css-derived-token?             : any/c -> boolean?
;;   Recognize a derived CSS token.
;; css-derived-token-raw          : css-derived-token? -> css-raw-token?
;;   Extract the underlying raw token.
;; css-derived-token-tags         : css-derived-token? -> (listof symbol?)
;;   Extract the derived classification tags.
;; derive-css-token               : css-raw-token? -> css-derived-token?
;;   Attach CSS-specific derived classifications to one raw token.
;; make-css-derived-classifier    : -> (css-raw-token? -> css-derived-token?)
;;   Construct a stateful CSS derived-token classifier with lightweight context.
;; css-derived-token-has-tag?     : css-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a classification tag.

(provide css-derived-token?
         css-derived-token-raw
         css-derived-token-tags
         derive-css-token
         make-css-derived-classifier
         css-derived-token-has-tag?)

(require racket/list
         racket/string
         "css-raw.rkt")

;; A raw CSS token paired with CSS-specific derived classifications.
(struct css-derived-token (raw tags) #:transparent)

;; A small practical set of CSS color function names.
(define css-color-functions
  '("rgb" "rgba" "hsl" "hsla" "hwb" "lab" "lch" "oklab" "oklch"
    "color" "color-mix" "device-cmyk" "light-dark"))

;; A small practical set of CSS gradient function names.
(define css-gradient-functions
  '("linear-gradient" "radial-gradient" "conic-gradient"
    "repeating-linear-gradient" "repeating-radial-gradient"
    "repeating-conic-gradient"))

;; custom-property-name? : string? -> boolean?
;;   Recognize a CSS custom property name.
(define (custom-property-name? text)
  (string-prefix? text "--"))

;; color-literal-hash? : string? -> boolean?
;;   Recognize a hash token that looks like a CSS color literal.
(define (color-literal-hash? text)
  (regexp-match? #px"^#(?:[0-9a-fA-F]{3}|[0-9a-fA-F]{4}|[0-9a-fA-F]{6}|[0-9a-fA-F]{8})$"
                 text))

;; known-color-function? : string? -> boolean?
;;   Recognize a CSS color function name.
(define (known-color-function? text)
  (member (string-downcase text) css-color-functions))

;; known-gradient-function? : string? -> boolean?
;;   Recognize a CSS gradient function name.
(define (known-gradient-function? text)
  (member (string-downcase text) css-gradient-functions))

;; raw-token->derived-tags : css-raw-token? -> (listof symbol?)
;;   Compute CSS-specific derived classification tags for one raw token.
(define (raw-token->base-derived-tags raw-token)
  (define kind (css-raw-token-kind raw-token))
  (define text (css-raw-token-text raw-token))
  (remove-duplicates
   (append
    (case kind
      [(at-keyword-token)
       '(at-rule-name)]
      [(hash-token)
       (cond
         [(color-literal-hash? text) '(color-literal)]
         [else                       '()])]
      [(function-token)
       (append
        '(function-name)
        (cond
          [(known-color-function? text)    '(color-function)]
          [else                            '()])
        (cond
          [(known-gradient-function? text) '(gradient-function)]
          [else                            '()]))]
      [(ident-token)
       (cond
         [(custom-property-name? text) '(custom-property-name)]
         [else                         '(property-name-candidate)])]
      [(dimension-token)
       (append
        '(numeric-literal)
        (cond
          [(regexp-match? #px"(px|rem|em|vw|vh|vmin|vmax|pt|pc|cm|mm|in|q|ch|ex)$" text)
           '(length-dimension)]
          [else
           '()]))]
      [(string-token)
       '(string-literal)]
      [(number-token percentage-token unicode-range-token)
       '(numeric-literal)]
      [else
       '()])
    (case kind
      [(bad-string-token bad-url-token) '(malformed-token)]
      [(unknown-raw-token) '(malformed-token)]
      [else                '()]))))

;; selector-role-kind? : symbol? -> boolean?
;;   Determine whether a raw token kind can play a selector/prelude role.
(define (selector-role-kind? kind)
  (case kind
    [(ident-token
      hash-token
      function-token
      string-token
      url-token
      number-token
      percentage-token
      dimension-token
      unicode-range-token)
     #t]
    [else
     #f]))

;; declaration-value-kind? : symbol? -> boolean?
;;   Determine whether a raw token kind can play a declaration-value role.
(define (declaration-value-kind? kind)
  (case kind
    [(ident-token
      at-keyword-token
      hash-token
      function-token
      string-token
      url-token
      number-token
      percentage-token
      dimension-token
      unicode-range-token)
     #t]
    [else
     #f]))

;; contextual-derived-tags : css-raw-token? symbol? -> (listof symbol?)
;;   Add lightweight context-sensitive tags for selectors and declarations.
(define (contextual-derived-tags raw-token mode)
  (define kind (css-raw-token-kind raw-token))
  (remove-duplicates
   (append
    (case kind
      [(ident-token)
       (case mode
         [(declaration-name) '(property-name)]
         [(rule-list)        '(selector-token)]
         [(declaration-value) '(declaration-value-token)]
         [else               '()])]
      [(at-keyword-token)
       (case mode
         [(rule-list) '(at-rule-name)]
         [(declaration-value) '(declaration-value-token)]
         [else        '()])]
      [(function-token)
       (append
        (case mode
          [(rule-list)         '(selector-token)]
          [(declaration-value) '(declaration-value-token)]
          [else                '()])
        '(function-name))]
      [else
       (append
        (cond
          [(and (eq? mode 'rule-list)
                (selector-role-kind? kind))
           '(selector-token)]
          [else
           '()])
        (cond
          [(and (eq? mode 'declaration-value)
                (declaration-value-kind? kind))
           '(declaration-value-token)]
          [else
           '()]))])
    '())))

;; derive-css-token : css-raw-token? -> css-derived-token?
;;   Attach CSS-specific derived classifications to one raw token.
(define (derive-css-token raw-token)
  (css-derived-token raw-token
                     (raw-token->base-derived-tags raw-token)))

;; make-css-derived-classifier : -> (css-raw-token? -> css-derived-token?)
;;   Construct a stateful CSS derived-token classifier with lightweight context.
(define (make-css-derived-classifier)
  (define mode                 'rule-list)
  (define block-stack          '())
  (define paren-depth          0)
  (define bracket-depth        0)
  (define prelude-has-at-rule? #f)
  (define (top-level-nesting?)
    (and (zero? paren-depth)
         (zero? bracket-depth)))
  (define (push-block! next-mode)
    (set! block-stack (cons mode block-stack))
    (set! mode next-mode)
    (set! prelude-has-at-rule? #f))
  (define (pop-block!)
    (cond
      [(pair? block-stack)
       (set! mode (car block-stack))
       (set! block-stack (cdr block-stack))]
      [else
       (set! mode 'rule-list)])
    (set! prelude-has-at-rule? #f))
  (lambda (raw-token)
    (define base-tags
      (raw-token->base-derived-tags raw-token))
    (define role-tags
      (contextual-derived-tags raw-token mode))
    (define derived-token
      (css-derived-token raw-token
                         (remove-duplicates
                          (append base-tags role-tags))))
    (define kind
      (css-raw-token-kind raw-token))
    (case kind
      [(at-keyword-token)
       (when (and (eq? mode 'rule-list)
                  (top-level-nesting?))
         (set! prelude-has-at-rule? #t))]
      [(open-paren-token)
       (set! paren-depth (add1 paren-depth))]
      [(close-paren-token)
       (when (positive? paren-depth)
         (set! paren-depth (sub1 paren-depth)))]
      [(open-bracket-token)
       (set! bracket-depth (add1 bracket-depth))]
      [(close-bracket-token)
       (when (positive? bracket-depth)
         (set! bracket-depth (sub1 bracket-depth)))]
      [(open-brace-token)
       (when (top-level-nesting?)
         (case mode
           [(rule-list)
            (cond
              [prelude-has-at-rule? (push-block! 'rule-list)]
              [else                 (push-block! 'declaration-name)])]
           [(declaration-name declaration-value)
            (push-block! 'rule-list)]
           [else
            (push-block! 'rule-list)]))]
      [(close-brace-token)
       (when (top-level-nesting?)
         (pop-block!))]
      [(colon-token)
       (when (and (eq? mode 'declaration-name)
                  (top-level-nesting?))
         (set! mode 'declaration-value))]
      [(semicolon-token)
       (when (top-level-nesting?)
         (case mode
           [(rule-list)
            (set! prelude-has-at-rule? #f)]
           [(declaration-value)
            (set! mode 'declaration-name)]
           [else
            (void)]))]
      [else
       (void)])
    derived-token))

;; css-derived-token-has-tag? : css-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a classification tag.
(define (css-derived-token-has-tag? derived-token tag)
  (member tag (css-derived-token-tags derived-token)))
