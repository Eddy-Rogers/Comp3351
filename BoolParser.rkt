#lang racket

(require parser-tools/yacc
         (prefix-in lex: parser-tools/lex)
         "BoolLanguage.rkt"
         "BooleanAbstractSyntaxTree.rkt")

(provide (all-defined-out))

(define myparser
  (parser
   (start expr)
   (end EOF)
   (tokens bool-values
           parens
           bool-operators
           end-of-file)
   (grammar
    (expr [(TRUE) boolean-value true]
          [(FALSE) boolean-value false]
          [(LEFTPAREN NOT expr RIGHTPAREN) (not-expr $3)]
          [(LEFTPAREN expr AND expr RIGHTPAREN) (and-expr $2 $4)]
          [(LEFTPAREN expr OR expr RIGHTPAREN) (or-expr $2 $4)]
          ))))

(Define (parse in)
        (myparser (get-tokenizer in)))

(define (parsestr str)
  (let ([in (open-input-string str)])
    (parse in)))

(define (parsefile filename)
  (let ([in (open-input-file filename)])
    (parse in)))