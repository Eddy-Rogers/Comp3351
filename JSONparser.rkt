;author @Eddy Rogers
#lang racket

(require parser-tools/yacc
         (prefix-in lex: parser-tools/lex)
         "JSONlexer.rkt")

(provide (all-defined-out))

;Structs for values in the JSON files (booleans, nulls, numbers, and strings)
(struct TrueVal (value) #:transparent)
(struct FalseVal (value) #:transparent)
(struct NullVal (value) #:transparent)
(struct StrVal (value) #:transparent)
(struct NumVal (value) #:transparent)

;Structs for non values in the JSON files (StrJSONPairs, Objects, and Arrays)
(struct StrJSONPair (string json) #:transparent) 
(struct ObjVal (list-of-strjsonpairs) #:transparent)
(struct Array (list-of-json-elements) #:transparent)

(define myparser
  (parser
      (start value)
      (end EOF)
      ;Tokens corresponding with all possible non-empty tokens in the lexer
      (tokens bool-values
              nulls
              brackets
              quotations
              commas
              connectives
              strings
              numbers
              end-of-file)
      (error (lambda (tok-ok? tok-name tok-value)
               (printf "Parser error: token ~a value ~a"
                           tok-name
                           tok-value)))
      (grammar
          (value
                ;Values consist of booleans, nulls, strings, and numbers
                [(TRUE) (TrueVal true)]
                [(FALSE) (FalseVal false)]
                [(NULL) (NullVal null)]
                [(STRING) (StrVal $1)]
                [(NUMBER) (NumVal $1)]
                ;It also includes empty arrays and objects, as well as the initial string value pairs in objects and array
                [(LEFTCRLY object) (ObjVal $2)]
                [(LEFTSQR array) (Array $2)]
                [(LEFTCRLY RIGHTCRLY) (ObjVal '())]
                [(LEFTSQR RIGHTSQR) (Array '())])
          ;Objects include a list of values appended with an indefinite amount of more values, seperated in the lexer by commas
          (array [(value COMMA array) (cons $1 $3)]
                 [(value RIGHTSQR) (list $1)])
          ;Objects include a list of string value pairs appended with an indefinite amount of more objects, seperated in the lexer by commas
          (object [(member COMMA object) (cons $1 $3)]
                 [(member RIGHTCRLY) (list $1)])
          ;A member consists of one case - a StrJSONPair
          (member [(STRING COLON value) (StrJSONPair $1 $3)])
      )))

(define (parse in)
  (myparser (get-tokenizer in)))

(define (parsestr str)
  (let ([in (open-input-string str)])
    (parse in)))

(define (parsefile filename)
  (let ([in (open-input-file filename)])
    (parse in)))