#lang racket

(struct TrueVal (value) #:transparent)
(struct FalseVal (value) #:transparent)
(struct NullVal (value) #:transparent)
(struct StrVal (value) #:transparent)
(struct NumVal (value) #:transparent)

(struct StrJSONPair (string json) #:transparent) 
(struct ObjVal (list-of-strjsonpairs) #:transparent)
(struct Array (list-of-json-elements) #:transparent)