#lang racket/base

;;;
;;; Swift Lexer
;;;
;;
;; Public entry points for the Swift lexer.

;; make-swift-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Swift lexer.
;; make-swift-derived-lexer : -> (input-port? -> (or/c swift-derived-token? 'eof))
;;   Construct a port-based Swift lexer that returns derived Swift token values.
;; swift-derived-token?     : any/c -> boolean?
;;   Recognize a derived Swift token value returned by the derived-token API.
;; swift-derived-token-tags : swift-derived-token? -> (listof symbol?)
;;   Extract the Swift-specific classification tags for one derived token.
;; swift-derived-token-has-tag? : swift-derived-token? symbol? -> boolean?
;;   Determine whether a derived Swift token has a given classification tag.
;; swift-derived-token-text : swift-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
;; swift-derived-token-start : swift-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; swift-derived-token-end  : swift-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; swift-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Swift string using the Swift lexer.
;; swift-string->derived-tokens : string? -> (listof swift-derived-token?)
;;   Tokenize an entire Swift string into derived Swift token values.
;; swift-profiles           : immutable-hash?
;;   Profile defaults for the public Swift lexer.

(provide make-swift-lexer
         make-swift-derived-lexer
         swift-derived-token?
         swift-derived-token-tags
         swift-derived-token-has-tag?
         swift-derived-token-text
         swift-derived-token-start
         swift-derived-token-end
         swift-string->tokens
         swift-string->derived-tokens
         swift-profiles)

(require parser-tools/lex
         racket/list
         "private/config.rkt"
         "private/parser-tools-compat.rkt"
         (rename-in "private/swift-derived.rkt"
                    [swift-derived-token? private-swift-derived-token?]
                    [swift-derived-token-tags private-swift-derived-token-tags]
                    [swift-derived-token-has-tag? private-swift-derived-token-has-tag?]
                    [swift-derived-token-text private-swift-derived-token-text]
                    [swift-derived-token-start private-swift-derived-token-start]
                    [swift-derived-token-end private-swift-derived-token-end]
                    [make-swift-derived-reader private-make-swift-derived-reader])
         "private/swift-tokenize.rkt"
         "token.rkt")

(define swift-profiles
  swift-profile-defaults)

;; swift-derived-token? : any/c -> boolean?
;;   Recognize a derived Swift token value returned by the derived-token API.
(define (swift-derived-token? v)
  (private-swift-derived-token? v))

;; swift-derived-token-tags : swift-derived-token? -> (listof symbol?)
;;   Extract the Swift-specific classification tags for one derived token.
(define (swift-derived-token-tags token)
  (private-swift-derived-token-tags token))

;; swift-derived-token-has-tag? : swift-derived-token? symbol? -> boolean?
;;   Determine whether a derived Swift token has a given classification tag.
(define (swift-derived-token-has-tag? token tag)
  (private-swift-derived-token-has-tag? token tag))

;; swift-derived-token-text : swift-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
(define (swift-derived-token-text token)
  (private-swift-derived-token-text token))

;; swift-derived-token-start : swift-derived-token? -> position?
;;   Extract the starting source position for one derived token.
(define (swift-derived-token-start token)
  (private-swift-derived-token-start token))

;; swift-derived-token-end : swift-derived-token? -> position?
;;   Extract the ending source position for one derived token.
(define (swift-derived-token-end token)
  (private-swift-derived-token-end token))

;; make-swift-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Swift lexer.
(define (make-swift-lexer #:profile          [profile 'coloring]
                          #:trivia           [trivia 'profile-default]
                          #:source-positions [source-positions 'profile-default])
  (define config
    (make-swift-config #:profile          profile
                       #:trivia           trivia
                       #:source-positions source-positions))
  (make-swift-token-reader config))

;; make-swift-derived-lexer : -> (input-port? -> (or/c swift-derived-token? 'eof))
;;   Construct a port-based Swift lexer that returns derived token values.
(define (make-swift-derived-lexer)
  (private-make-swift-derived-reader))

;; swift-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Swift string using the projected token API.
(define (swift-string->tokens source
                              #:profile          [profile 'coloring]
                              #:trivia           [trivia 'profile-default]
                              #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-swift-lexer #:profile          profile
                      #:trivia           trivia
                      #:source-positions source-positions))
  (define in
    (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token
      (lexer in))
    (cond
      [(eof-token? token)
       (reverse (cons token tokens))]
      [else
       (loop (cons token tokens))])))

;; swift-string->derived-tokens : string? -> (listof swift-derived-token?)
;;   Tokenize an entire Swift string into derived Swift token values.
(define (swift-string->derived-tokens source)
  (define lexer
    (make-swift-derived-lexer))
  (define in
    (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token
      (lexer in))
    (cond
      [(eq? token 'eof)
       (reverse tokens)]
      [else
       (loop (cons token tokens))])))

(module+ test
  (require rackunit)

  ;; contiguous-derived-stream? : (listof swift-derived-token?) -> boolean?
  ;;   Determine whether adjacent derived tokens cover the source contiguously.
  (define (contiguous-derived-stream? tokens)
    (for/and ([left  (in-list tokens)]
              [right (in-list (cdr tokens))])
      (= (position-offset (swift-derived-token-end left))
         (position-offset (swift-derived-token-start right)))))

  ;; first-token-before-rest? : (-> any) string? string? -> any
  ;;   Read the first token before the second chunk is written.
  (define (first-token-before-rest? make-lexer first-chunk rest-chunk)
    (define lexer
      (make-lexer))
    (define-values (in out)
      (make-pipe))
    (write-string first-chunk out)
    (flush-output out)
    (define result-channel
      (make-channel))
    (thread
     (lambda ()
       (channel-put result-channel (lexer in))))
    (define token
      (sync/timeout 1 result-channel))
    (write-string rest-chunk out)
    (close-output-port out)
    token)

  (define sample-source
    "import UIKit\n\nclass ViewController: UIViewController {\n    @IBOutlet weak var label: UILabel!\n    func answer(_ value: String) -> String {\n        return \"Hello, \\(value)\"\n    }\n}\n")
  (define sample-tokens
    (swift-string->tokens sample-source
                          #:profile 'coloring
                          #:source-positions #f))
  (define compiler-tokens
    (swift-string->tokens sample-source
                          #:profile 'compiler
                          #:source-positions #f))
  (define sample-derived
    (swift-string->derived-tokens sample-source))
  (define nested-comment-source
    "/* outer /* inner */ outer */\n")
  (define nested-comment-derived
    (swift-string->derived-tokens nested-comment-source))
  (define multiline-string-source
    "let text = \"\"\"hello\nworld\"\"\"\n")
  (define multiline-string-derived
    (swift-string->derived-tokens multiline-string-source))
  (define malformed-coloring
    (swift-string->tokens "let text = \"unterminated\n"
                          #:profile 'coloring
                          #:source-positions #f))
  (define malformed-compiler-thunk
    (lambda ()
      (swift-string->tokens "let text = \"unterminated\n"
                            #:profile 'compiler
                            #:source-positions #f)))
  (define crlf-source
    "import UIKit\r\nlet value = 42\r\n")
  (define crlf-derived
    (swift-string->derived-tokens crlf-source))
  (define first-streaming-token
    (first-token-before-rest? make-swift-derived-lexer
                              "import UI"
                              "Kit\n"))

  (define keyword-token
    (findf (lambda (token)
             (swift-derived-token-has-tag? token 'swift-keyword))
           sample-derived))
  (define attribute-token
    (findf (lambda (token)
             (swift-derived-token-has-tag? token 'swift-attribute))
           sample-derived))
  (define string-token
    (findf (lambda (token)
             (swift-derived-token-has-tag? token 'swift-string-literal))
           sample-derived))
  (define operator-token
    (findf (lambda (token)
             (swift-derived-token-has-tag? token 'swift-operator))
           sample-derived))
  (define comment-token
    (findf (lambda (token)
             (swift-derived-token-has-tag? token 'swift-comment))
           nested-comment-derived))
  (define multiline-string-token
    (findf (lambda (token)
             (swift-derived-token-has-tag? token 'swift-string-literal))
           multiline-string-derived))

  (check-equal? (take (map lexer-token-name sample-tokens) 8)
                '(keyword whitespace identifier whitespace whitespace keyword whitespace identifier))
  (check-equal? (map lexer-token-name compiler-tokens)
                '(keyword
                  identifier
                  keyword
                  identifier
                  delimiter
                  identifier
                  delimiter
                  keyword
                  keyword
                  keyword
                  identifier
                  delimiter
                  identifier
                  operator
                  keyword
                  identifier
                  delimiter
                  identifier
                  identifier
                  delimiter
                  identifier
                  delimiter
                  operator
                  identifier
                  delimiter
                  keyword
                  literal
                  delimiter
                  delimiter
                  eof))
  (check-not-false keyword-token)
  (check-not-false attribute-token)
  (check-not-false string-token)
  (check-not-false operator-token)
  (check-not-false comment-token)
  (check-not-false multiline-string-token)
  (check-not-false first-streaming-token)
  (check-equal? (map lexer-token-name malformed-coloring)
                '(keyword whitespace identifier whitespace operator whitespace unknown whitespace eof))
  (check-exn exn:fail:read? malformed-compiler-thunk)
  (check-true (contiguous-derived-stream? sample-derived))
  (check-equal? (apply string-append (map swift-derived-token-text sample-derived))
                sample-source)
  (check-equal? (apply string-append (map swift-derived-token-text nested-comment-derived))
                nested-comment-source)
  (check-equal? (apply string-append (map swift-derived-token-text multiline-string-derived))
                multiline-string-source)
  (check-equal? (apply string-append (map swift-derived-token-text crlf-derived))
                crlf-source)
  (check-equal? (swift-derived-token-text first-streaming-token)
                "import"))
