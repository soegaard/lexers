#lang racket/base

;;;
;;; JavaScript Derived Classifications
;;;
;;
;; Derived JavaScript-specific classifications layered on top of raw tokens.

;; javascript-derived-token?             : any/c -> boolean?
;;   Recognize a derived JavaScript token.
;; javascript-derived-token-raw          : javascript-derived-token? -> javascript-raw-token?
;;   Extract the underlying raw token.
;; javascript-derived-token-tags         : javascript-derived-token? -> (listof symbol?)
;;   Extract the derived classification tags.
;; derive-javascript-token               : javascript-raw-token? -> javascript-derived-token?
;;   Attach JavaScript-specific derived classifications to one raw token.
;; make-javascript-derived-classifier    : -> (javascript-raw-token? -> javascript-derived-token?)
;;   Construct a stateful JavaScript derived-token classifier with lightweight context.
;; javascript-derived-token-has-tag?     : javascript-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a classification tag.

(provide javascript-derived-token?
         javascript-derived-token-raw
         javascript-derived-token-tags
         derive-javascript-token
         make-javascript-derived-classifier
         javascript-derived-token-has-tag?)

(require racket/list
         "javascript-raw.rkt")

(struct javascript-derived-token (raw tags) #:transparent)

(define javascript-keywords
  '("break" "case" "catch" "class" "const" "continue" "debugger" "default"
    "delete" "do" "else" "export" "extends" "false" "finally" "for"
    "function" "if" "import" "in" "instanceof" "let" "new" "null" "of"
    "return" "static" "super" "switch" "this" "throw" "true" "try"
    "typeof" "var" "void" "while" "with" "yield" "await"))

;; javascript-keyword? : string? -> boolean?
;;   Recognize a keyword in the small initial JavaScript subset.
(define (javascript-keyword? text)
  (member text javascript-keywords))

;; raw-token->base-derived-tags : javascript-raw-token? -> (listof symbol?)
;;   Compute JavaScript-specific derived classification tags for one raw token.
(define (raw-token->base-derived-tags raw-token)
  (define kind (javascript-raw-token-kind raw-token))
  (define text (javascript-raw-token-text raw-token))
  (remove-duplicates
   (append
    (case kind
      [(identifier-token)
       (cond
         [(javascript-keyword? text) '(keyword)]
         [else                       '(identifier)])]
      [(private-name-token) '(private-name)]
      [(method-name-token) '(identifier method-name)]
      [(jsx-tag-name-token) '(identifier jsx-tag-name)]
      [(jsx-closing-tag-name-token) '(identifier jsx-closing-tag-name)]
      [(jsx-attribute-name-token) '(identifier jsx-attribute-name)]
      [(jsx-text-token) '(jsx-text)]
      [(jsx-fragment-boundary-token) '(jsx-fragment-boundary)]
      [(jsx-interpolation-start-token
        jsx-interpolation-end-token)
       '(jsx-interpolation-boundary)]
      [(string-token) '(string-literal)]
      [(number-token) '(numeric-literal)]
      [(regex-token) '(regex-literal)]
      [(template-chunk-token) '(template-literal template-chunk)]
      [(template-start-token
        template-end-token) '(template-literal)]
      [(template-interpolation-start-token
        template-interpolation-end-token)
       '(template-literal template-interpolation-boundary)]
      [(line-comment-token block-comment-token) '(comment)]
      [else '()])
    (case kind
      [(bad-string-token unknown-raw-token) '(malformed-token)]
      [else                                 '()]))))

;; contextual-derived-tags : javascript-raw-token? symbol? (listof symbol?) symbol? -> (listof symbol?)
;;   Add lightweight JavaScript role tags using parser-lite context.
(define (contextual-derived-tags raw-token decl-state paren-stack brace-stack previous-token)
  (define kind
    (javascript-raw-token-kind raw-token))
  (define text
    (javascript-raw-token-text raw-token))
  (define keyword?
    (and (eq? kind 'identifier-token)
         (javascript-keyword? text)))
  (define previous-text
    (and previous-token
         (javascript-raw-token-text previous-token)))
  (define in-params?
    (and (pair? paren-stack)
         (eq? (car paren-stack) 'params)))
  (define in-class-body?
    (and (pair? brace-stack)
         (eq? (car brace-stack) 'class-body)))
  (define in-object-literal?
    (and (pair? brace-stack)
         (eq? (car brace-stack) 'object-literal)))
  (remove-duplicates
   (append
    (cond
      [(and (memq kind '(identifier-token private-name-token))
            (not keyword?)
            (memq decl-state '(var-name function-name class-name)))
       '(declaration-name)]
      [(or keyword?
           (not (memq kind '(identifier-token private-name-token method-name-token string-token number-token))))
       '()]
      [(and (memq kind '(identifier-token private-name-token string-token number-token))
            in-object-literal?
            previous-token
            (eq? (javascript-raw-token-kind previous-token) 'delimiter-token)
            (member previous-text '("{" ",")))
       '(object-key)]
      [(and (eq? kind 'identifier-token)
            in-params?)
       '(parameter-name)]
      [else
       '()])
    (cond
      [(and keyword?
            (string=? text "static")
            in-class-body?)
       '(static-keyword-usage)]
      [(and (memq kind '(identifier-token private-name-token method-name-token))
            previous-text
            (string=? previous-text "."))
       '(property-name)]
      [else
       '()]))))

;; derive-javascript-token : javascript-raw-token? -> javascript-derived-token?
;;   Attach JavaScript-specific derived classifications to one raw token.
(define (derive-javascript-token raw-token)
  (javascript-derived-token raw-token
                            (raw-token->base-derived-tags raw-token)))

;; make-javascript-derived-classifier : -> (javascript-raw-token? -> javascript-derived-token?)
;;   Construct a stateful JavaScript derived-token classifier with lightweight context.
(define (make-javascript-derived-classifier)
  (define decl-state            'none)
  (define paren-stack           '())
  (define brace-stack           '())
  (define expect-params?        #f)
  (define pending-brace-context #f)
  (define pending-method-token  #f)
  (define previous-token        #f)
  (lambda (raw-token)
    (define base-tags
      (remove-duplicates
       (append (raw-token->base-derived-tags raw-token)
               (contextual-derived-tags raw-token
                                        decl-state
                                        paren-stack
                                        brace-stack
                                        previous-token))))
    (define tags
      (cond
        [(and pending-method-token
              (eq? raw-token pending-method-token)
              (eq? (javascript-raw-token-kind raw-token) 'identifier-token))
         (cons 'method-name base-tags)]
        [else
         base-tags]))
    (define derived-token
      (javascript-derived-token raw-token tags))
    (define kind
      (javascript-raw-token-kind raw-token))
    (define text
      (javascript-raw-token-text raw-token))
    (define keyword?
      (and (eq? kind 'identifier-token)
           (javascript-keyword? text)))
    (cond
      [(and keyword? (member text '("const" "let" "var")))
       (set! decl-state 'var-name)]
      [(and keyword? (string=? text "function"))
       (set! decl-state 'function-name)
       (set! expect-params? #t)
       (set! pending-brace-context 'function-body)]
      [(and keyword? (string=? text "class"))
       (set! decl-state 'class-name)
       (set! pending-brace-context 'class-body)]
      [(member 'declaration-name tags)
       (set! decl-state 'none)]
      [(and (eq? kind 'operator-token)
            (string=? text ",")
            (eq? decl-state 'var-name))
       (set! decl-state 'var-name)]
      [(and (eq? kind 'operator-token)
            (not (string=? text ","))
            (eq? decl-state 'var-name))
       (set! decl-state 'none)]
      [else
       (void)])
    (case kind
      [(delimiter-token)
       (cond
         [(string=? text "(")
          (when pending-method-token
            (set! pending-method-token #f))
          (set! paren-stack
                (cons (if expect-params?
                          'params
                          'group)
                      paren-stack))
          (set! expect-params? #f)]
         [(string=? text ")")
          (when (pair? paren-stack)
            (set! paren-stack (cdr paren-stack)))]
         [(string=? text "{")
          (define brace-context
            (or pending-brace-context
                (and previous-token
                     (cond
                       [(and (eq? (javascript-raw-token-kind previous-token) 'operator-token)
                             (member (javascript-raw-token-text previous-token)
                                     '("=" ":")))
                        'object-literal]
                       [(and (eq? (javascript-raw-token-kind previous-token) 'identifier-token)
                             (javascript-keyword? (javascript-raw-token-text previous-token))
                             (member (javascript-raw-token-text previous-token)
                                     '("return")))
                        'object-literal]
                       [else
                        #f]))
                'block))
          (set! brace-stack
                (cons brace-context
                      brace-stack))
          (set! pending-brace-context #f)
          (set! decl-state 'none)]
         [(string=? text "}")
          (when (pair? brace-stack)
            (set! brace-stack (cdr brace-stack)))
          (set! decl-state 'none)
          (set! expect-params? #f)
          (set! pending-brace-context #f)]
         [(string=? text ";")
          (set! decl-state 'none)]
         [else
          (void)])]
      [else
       (void)])
    (when (and previous-token
               (eq? (javascript-raw-token-kind previous-token) 'delimiter-token)
               (string=? (javascript-raw-token-text previous-token) ".")
               (memq kind '(identifier-token private-name-token)))
      (set! pending-method-token raw-token))
    (when (and pending-method-token
               (not (eq? raw-token pending-method-token))
               (not (and (eq? kind 'delimiter-token)
                         (string=? text "("))))
      (set! pending-method-token #f))
    (unless (memq kind '(whitespace-token line-comment-token block-comment-token))
      (set! previous-token raw-token))
    derived-token))

;; javascript-derived-token-has-tag? : javascript-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a classification tag.
(define (javascript-derived-token-has-tag? derived-token tag)
  (member tag (javascript-derived-token-tags derived-token)))
