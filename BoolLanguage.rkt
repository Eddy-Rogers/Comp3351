#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide (all-defined-out))

(define-empty-tokens bool-values (TRUE FALSE))
(define-empty-tokens parens (LEFTPAREN RIGHTPAREN))
(define-empty-tokens bool-operators (AND OR NOT))
(define-empty-tokens end-of-file (EOF))

(define mylexer
  (lexer
   [#\(       (token-LEFTPAREN)]
   [#\)       (token-RIGHTPAREN)]
   [(:or "and" "And" "AND")  (token-AND)]
   ["or"      (token-OR)]
   ["not"     (token-NOT)]
   ["true"    (token-TRUE)]
   ["false"   (token-FALSE)]
   [whitespace (mylexer input-port)]
   [(eof)     (token-EOF)]
   ))

(define (get-tokenizer in)
  (lambda () (mylexer in)))

(define (lex in)
  (let ([tokenizer (get-tokenizer in)])
    (define (lex-function)
      (let ([tok (tokenizer)])
        (cond
          [(eq? tok (token-EOF)) null]
          [else (cons tok (lex-function))])))
    (lex-function)))
    
(define (lexstr str)
  (lex (open-input-string str)))

(define example (open-input-string "(not true)"))