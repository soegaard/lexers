#lang racket/base

;;;
;;; Plist Lexer
;;;
;;
;; Public entry points for the XML property-list lexer.

;; make-plist-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based plist lexer.
;; make-plist-derived-lexer : -> (input-port? -> (or/c plist-derived-token? 'eof))
;;   Construct a port-based plist lexer that returns derived token values.
;; plist-derived-token?     : any/c -> boolean?
;;   Recognize a derived plist token value returned by the derived-token API.
;; plist-derived-token-tags : plist-derived-token? -> (listof symbol?)
;;   Extract the plist-specific classification tags for one derived token.
;; plist-derived-token-has-tag? : plist-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; plist-derived-token-text : plist-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
;; plist-derived-token-start : plist-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; plist-derived-token-end  : plist-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; plist-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire plist string using the plist lexer.
;; plist-string->derived-tokens : string? -> (listof plist-derived-token?)
;;   Tokenize an entire plist string into derived plist token values.
;; plist-profiles           : immutable-hash?
;;   Profile defaults for the public plist lexer.

(provide make-plist-lexer
         make-plist-derived-lexer
         plist-derived-token?
         plist-derived-token-tags
         plist-derived-token-has-tag?
         plist-derived-token-text
         plist-derived-token-start
         plist-derived-token-end
         plist-string->tokens
         plist-string->derived-tokens
         plist-profiles)

(require parser-tools/lex
         racket/list
         "private/config.rkt"
         (rename-in "private/plist-derived.rkt"
                    [plist-derived-token? private-plist-derived-token?]
                    [plist-derived-token-tags private-plist-derived-token-tags]
                    [plist-derived-token-has-tag? private-plist-derived-token-has-tag?]
                    [plist-derived-token-text private-plist-derived-token-text]
                    [plist-derived-token-start private-plist-derived-token-start]
                    [plist-derived-token-end private-plist-derived-token-end]
                    [make-plist-derived-reader private-make-plist-derived-reader])
         "private/plist-tokenize.rkt"
         "token.rkt")

(define plist-profiles
  plist-profile-defaults)

(define (plist-derived-token? v)
  (private-plist-derived-token? v))

(define (plist-derived-token-tags token)
  (private-plist-derived-token-tags token))

(define (plist-derived-token-has-tag? token tag)
  (private-plist-derived-token-has-tag? token tag))

(define (plist-derived-token-text token)
  (private-plist-derived-token-text token))

(define (plist-derived-token-start token)
  (private-plist-derived-token-start token))

(define (plist-derived-token-end token)
  (private-plist-derived-token-end token))

(define (make-plist-lexer #:profile          [profile 'coloring]
                          #:trivia           [trivia 'profile-default]
                          #:source-positions [source-positions 'profile-default])
  (define config
    (make-plist-config #:profile          profile
                       #:trivia           trivia
                       #:source-positions source-positions))
  (make-plist-token-reader config))

(define (make-plist-derived-lexer)
  (private-make-plist-derived-reader))

(define (plist-string->tokens source
                              #:profile          [profile 'coloring]
                              #:trivia           [trivia 'profile-default]
                              #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-plist-lexer #:profile          profile
                      #:trivia           trivia
                      #:source-positions source-positions))
  (define in
    (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token
      (lexer in))
    (cond
      [(lexer-token-eof? token)
       (reverse (cons token tokens))]
      [else
       (loop (cons token tokens))])))

(define (plist-string->derived-tokens source)
  (define lexer
    (make-plist-derived-lexer))
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

  (define (contiguous-derived-stream? tokens)
    (for/and ([left  (in-list tokens)]
              [right (in-list (cdr tokens))])
      (= (position-offset (plist-derived-token-end left))
         (position-offset (plist-derived-token-start right)))))

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
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n<plist version=\"1.0\"><dict><key>CFBundleName</key><string>Lexers &amp; More</string><true/></dict></plist>\n")
  (define sample-derived
    (plist-string->derived-tokens sample-source))
  (define sample-tokens
    (plist-string->tokens sample-source
                          #:profile 'coloring
                          #:source-positions #f))
  (define compiler-tokens
    (plist-string->tokens sample-source
                          #:profile 'compiler
                          #:source-positions #f))
  (define crlf-source
    "<?xml version=\"1.0\"?>\r\n<plist version=\"1.0\"><dict><key>A</key><integer>1</integer></dict></plist>\r\n")
  (define crlf-derived
    (plist-string->derived-tokens crlf-source))
  (define malformed-attribute-source
    "<plist version=1.0></plist>")
  (define malformed-attribute-derived
    (plist-string->derived-tokens malformed-attribute-source))
  (define first-streaming-token
    (first-token-before-rest? make-plist-derived-lexer
                              "<?xml version=\"1.0\"?>\n"
                              "<plist version=\"1.0\"><dict/></plist>\n"))
  (define key-token
    (findf (lambda (token)
             (plist-derived-token-has-tag? token 'plist-key-text))
           sample-derived))
  (define string-token
    (findf (lambda (token)
             (plist-derived-token-has-tag? token 'plist-string-text))
           sample-derived))
  (define doctype-token
    (findf (lambda (token)
             (plist-derived-token-has-tag? token 'plist-doctype))
           sample-derived))
  (define xml-token
    (findf (lambda (token)
             (plist-derived-token-has-tag? token 'plist-processing-instruction))
           sample-derived))
  (define malformed-attribute-token
    (findf (lambda (token)
             (and (plist-derived-token-has-tag? token 'plist-attribute-value)
                  (plist-derived-token-has-tag? token 'malformed-token)))
           malformed-attribute-derived))
  (define entity-token
    (findf (lambda (token)
             (plist-derived-token-has-tag? token 'plist-entity))
           sample-derived))

  (check-equal? (take (map lexer-token-name sample-tokens) 4)
                '(keyword whitespace keyword whitespace))
  (check-equal? (last (map lexer-token-name compiler-tokens))
                'eof)
  (check-not-false key-token)
  (check-not-false string-token)
  (check-not-false doctype-token)
  (check-not-false xml-token)
  (check-not-false entity-token)
  (check-equal? (plist-derived-token-text key-token)
                "CFBundleName")
  (check-equal? (plist-derived-token-text string-token)
                "Lexers ")
  (check-equal? (plist-derived-token-text entity-token)
                "&amp;")
  (check-not-false malformed-attribute-token)
  (check-equal? (plist-derived-token-text malformed-attribute-token)
                "1.0")
  (check-true (contiguous-derived-stream? sample-derived))
  (check-equal? (apply string-append (map plist-derived-token-text sample-derived))
                sample-source)
  (check-equal? (apply string-append (map plist-derived-token-text crlf-derived))
                crlf-source)
  (check-not-false first-streaming-token))
