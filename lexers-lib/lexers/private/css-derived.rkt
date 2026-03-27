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
;; css-derived-token-has-tag?     : css-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a classification tag.

(provide css-derived-token?
         css-derived-token-raw
         css-derived-token-tags
         derive-css-token
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
(define (raw-token->derived-tags raw-token)
  (define kind (css-raw-token-kind raw-token))
  (define text (css-raw-token-text raw-token))
  (remove-duplicates
   (append
    (case kind
      [(hash-token)
       (cond
         [(color-literal-hash? text) '(color-literal)]
         [else                       '()])]
      [(function-token)
       (append
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
       (cond
         [(regexp-match? #px"(px|rem|em|vw|vh|vmin|vmax|pt|pc|cm|mm|in|q|ch|ex)$" text)
          '(length-dimension)]
         [else
          '()])]
      [else
       '()])
    (case kind
      [(bad-string-token) '(malformed-token)]
      [(unknown-raw-token) '(malformed-token)]
      [else                '()]))))

;; derive-css-token : css-raw-token? -> css-derived-token?
;;   Attach CSS-specific derived classifications to one raw token.
(define (derive-css-token raw-token)
  (css-derived-token raw-token
                     (raw-token->derived-tags raw-token)))

;; css-derived-token-has-tag? : css-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a classification tag.
(define (css-derived-token-has-tag? derived-token tag)
  (member tag (css-derived-token-tags derived-token)))
