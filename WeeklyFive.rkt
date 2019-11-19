; @author Eddy Rogers
; 11/18/2019

#lang racket

(require "JSONAbstractSyntaxTree.rkt"
         "JSONparser.rkt")

;Method Name: objectContains?
;Function Type: boolean
;Function Parameters: A JSON object and a string
;Return Value: Whether or not the JSON object contains the string as field within it
(define (objectContains? object fieldName)
  (match object
    [(ObjVal listOfPairs)
     (objectContainsHelper listOfPairs fieldName)]
    [_ (error "Not an Object!")]))
     
;Method Name: objectContainsHelper
;Function Type: boolean
;Function Parameters: A JSON object and a string
;Return Value: Whether or not the JSON object contains the string as field within it
(define (objectContainsHelper jsonPairs fieldName)
  (if (empty? jsonPairs)
      #f
      (match (first jsonPairs)
        [(StrJSONPair string value)
         (if (string=? string fieldName)
             #t
             (objectContainsHelper (rest jsonPairs) fieldName))]
        [_ (error "Not a StrJsonPair!")])))
               
;Method Name: getField
;Function Type: A JSON Object
;Function Parameters: A JSON object and a string
;Return Value: The JSON object corresponding with the given field value
(define (getField object fieldName)
  (match object
    [(ObjVal listOfPairs)
     (ObjVal (getFieldHelper listOfPairs fieldName))]
    [_ (error "Not an Object!")]))

;Method Name: getFieldHelper
;Function Type: List
;Function Parameters: A JSON object and a string
;Return Value: A list of the values contained in the JSON object corresponding with the given field value
(define (getFieldHelper jsonPairs fieldName)
  (if (empty? jsonPairs)
      empty
      (match (first jsonPairs)
        [(StrJSONPair string value)
         (if (string=? string fieldName)
             value
             (getFieldHelper (rest jsonPairs) fieldName))]
        [_ (error "Not a StrJsonPair!")])))

;Method Name: filterKeys
;Function Type: A JSON Object
;Function Parameters: A function and a JSON object
;Return Value: A JSON object where the given function is used to get rid of keys with which the function is false
(define (filterKeys function object)
  (match object
    [(ObjVal listOfPairs)
     (ObjVal (filterKeysHelper function listOfPairs))]
    [_ (error "Not an Object!")]))

;Method Name: filterKeysHelper
;Function Type: List
;Function Parameters: A function and a JSON object
;Return Value: A list of values from the JSON object where the given function is used to get rid of keys with which the function is false
(define (filterKeysHelper function jsonPairs)
  (if (empty? jsonPairs)
      empty
      (match (first jsonPairs)
        [(StrJSONPair string value)
        (if (function string)
            (cons (first jsonPairs) (filterKeysHelper function (rest jsonPairs)))
            (filterKeysHelper function (rest jsonPairs)))]
        [_ (error "Not a StrJsonPair!")])))

;Method Name: keyCount
;Function Type: int
;Function Parameters: A JSON object
;Return Value: The amound of keys in the given JSON object
(define (keyCount object)
  (match object
    [(ObjVal listOfPairs)
     (keyCountHelper listOfPairs)]
    [_ (error "Not an Object!")]))

;Method Name: keyCountHelper
;Function Type: int
;Function Parameters: A list of JSON pairs
;Return Value: The amount of JSON pairs in the given list
(define (keyCountHelper jsonPairs)
  (if (empty? jsonPairs)
      0
      (match (first jsonPairs)
        [(StrJSONPair string value)
        (+ 1 (keyCountHelper (rest jsonPairs)))]
        [_ (error "Not a StrJsonPair!")])))

;Method Name: keyList
;Function Type: List
;Function Parameters: A JSON object
;Return Value: All of the key values contained in the given object
(define (keyList object)
  (match object
    [(ObjVal listOfPairs)
     (keyListHelper listOfPairs)]
    [_ (error "Not an Object!")]))

;Method Name: keyListHelper
;Function Type: List
;Function Parameters: A list of JSON pairs
;Return Value: A list with just the string (key) value of each given JSON pair
(define (keyListHelper jsonPairs)
  (if (empty? jsonPairs)
      empty
      (match (first jsonPairs)
        [(StrJSONPair string value)
        (cons string (keyListHelper (rest jsonPairs)))]
        [_ (error "Not a StrJsonPair!")])))

;Method Name: arrayLength
;Function Type: Int
;Function Parameters: An Array
;Return Value: The number of elements contained in the array
(define (arrayLength array)
  (match array
    [(Array listOfValues)
     (length listOfValues)]
    [_ (error "Not a Array!")]))

;Method Name: filterRange
;Function Type: An Array
;Function Parameters: Two integers (A low index and a high index) and an Array
;Return Value: An array containing only the values between the specified indeces
(define (filterRange low high array)
  (match array
    [(Array listOfValues)
     (Array (filterRangeHelper 0 low high listOfValues))]
    [_ (error "Not a Array!")]))

;Method Name: filterRangeHelper
;Function Type: List
;Function Parameters: Three integers (a current index, a low index and a high index) and an array
;Return Value: A list containing the elements in the array between the low and high indeces
(define (filterRangeHelper index low high array)
  (if (= high index)
      (list (first array))
      (if (< index low)
          (filterRangeHelper (+ index 1) low high (rest array))
          (cons (first array) (filterRangeHelper (+ index 1) low high (rest array))))))

;Method Name: filterArray
;Function Type: An Array
;Function Parameters: A function and an Array
;Return Value: An array where the function has been applied to each element, and those that returned false are removed
(define (filterArray function array)
  (match array
    [(Array listOfValues)
     (Array (filterArrayHelper function array))]
    [_ (error "Not an Array!")]))

;Method Name: filterArrayHelper
;Function Type: List
;Function Parameters: A function and an Array
;Return Value:  A list of all elements where the function has been applied to each element of the given Array, and those that returned false are removed
(define (filterArrayHelper function array)
  (if (empty? array)
      empty
      (if (function (first array))
          (cons (first array) (filterArrayHelper function (rest array)))
          (filterArrayHelper function (rest array)))))

;Method Name: extractElements
;Function Type: An Array
;Function Parameters: An array and a list of indeces
;Return Value: Only the elements of the array specified by the list of indeces provided
(define (extractElements array indeces)
  (match array
    [(Array listOfValues)
    (Array (extractElementsHelper listOfValues indeces))]
    [_ (error "Not an Array!")]))

;Method Name: extractElementsHelper
;Function Type: List
;Function Parameters: An array and a list of indeces
;Return Value: A list of elements from the given array, specified from the list of indeces provided
(define (extractElementsHelper array indeces)
  (if (empty? indeces)
      empty
      (cons (getElementAt (first indeces) array) (extractElementsHelper array (rest indeces)))))

;Method Name: getElementAt
;Function Type: A single value of the type contained in the list
;Function Parameters: An integer for the current index and a list
;Return Value: The element at the specified index
(define (getElementAt index array)
  (if (empty? array)
      (error "Index out of Bounds!")
      (if (= 0 index)
          (first array)
          (getElementAt (- index 1) (rest array)))))

;Method Name: toInt
;Function Type: Int
;Function Parameters: A string
;Return Value: The integer representation of the provided string
(define (toInt string)
  (match string
    [(NullVal val) 0]
    [(StrVal val) (string->number (substring val 1 (- (string-length val) 1)))]))

;Method Name: escapeCleanUp
;Function Type: String
;Function Parameters: A string
;Return Value: A string with the first and last characters taken off from the original
(define (escapeCleanUp stringVal)
  (match stringVal
    [(NullVal val) "0"]
    [(StrVal string) (substring string 1 (- (string-length string) 1))]))

;Method Name: increasingIncidents
;Function Type: A List of Strings
;Function Parameters: A filename as a string
;Return Value: Returns all diseases which has had an increasing number of cases from 2013 to 2017
(define (increasingIncidents fileName)
  (let ([openFile (open-input-file fileName)])
    (let ([parsedData (parse openFile)])
      (let ([dataArray (match (getField parsedData "\"data\"")
             [(ObjVal val) val])])
        (increasingIncidentsHelper (match dataArray
          [(Array val) val]))))))

;Method Name: increasingIncidentsHelper
;Function Type: A List of strings
;Function Parameters: A list of values corresponding to one disease
;Return Value: A list of strings with diseases and their case statistics whose cases have increased from 2013 to 2017
(define (increasingIncidentsHelper listValues)
  (if (empty? listValues)
      empty
      (letrec ([diseaseArray (first listValues)])
          (letrec ([diseaseData (match (extractElements diseaseArray (list 8 10 17 25))
                                  [(Array val) val])])
            (if (= 15 (toInt (list-ref diseaseData 1)))
                (if (> (toInt (list-ref diseaseData 2)) (toInt (list-ref diseaseData 3)))
                    (append (list (string-append (escapeCleanUp (list-ref diseaseData 0)) ": " (escapeCleanUp (list-ref diseaseData 3))
                                                 " cases in 2013, " (escapeCleanUp (list-ref diseaseData 2)) " cases in 2017")) (increasingIncidentsHelper (rest listValues)))
                    (increasingIncidentsHelper (rest listValues)))
                (increasingIncidentsHelper (rest listValues)))))))

;Method Name: strictlyIncreasing
;Function Type: A list of strings
;Function Parameters: A file name (string)
;Return Value: Returns all diseases which has had a strictly increasing number of cases from 2013 through 2017
(define (strictlyIncreasing fileName)
  (let ([openFile (open-input-file fileName)])
    (let ([parsedData (parse openFile)])
      (let ([dataArray (match (getField parsedData "\"data\"")
             [(ObjVal val) val])])
        (strictlyIncreasingHelper (match dataArray
          [(Array val) val]))))))

;Method Name: strictlyIncreasingHelper
;Function Type: A list of strings
;Function Parameters: A list of values corresponding to a single disease
;Return Value: A list of strings with diseases and their case statistics whose cases have strictly increased from 2013 through 2017
(define (strictlyIncreasingHelper listValues)
  (if (empty? listValues)
      empty
      (letrec ([diseaseArray (first listValues)])
          (letrec ([diseaseData (match (extractElements diseaseArray (list 8 10 17 19 21 23 25))
                                  [(Array val) val])])
            (if (= 15 (toInt (list-ref diseaseData 1)))
                (if (and (> (toInt (list-ref diseaseData 2)) (toInt (list-ref diseaseData 3)))
                         (> (toInt (list-ref diseaseData 3)) (toInt (list-ref diseaseData 4)))
                         (> (toInt (list-ref diseaseData 4)) (toInt (list-ref diseaseData 5)))
                         (> (toInt (list-ref diseaseData 5)) (toInt (list-ref diseaseData 6))))
                    (append (list (string-append (escapeCleanUp (list-ref diseaseData 0)) ": "
                                                 (escapeCleanUp (list-ref diseaseData 6)) " cases in 2013, " (escapeCleanUp (list-ref diseaseData 5)) " cases in 2014, "
                                                 (escapeCleanUp (list-ref diseaseData 4)) " cases in 2015, " (escapeCleanUp (list-ref diseaseData 3)) " cases in 2016, "
                                                 (escapeCleanUp (list-ref diseaseData 2)) " cases in 2017")) (strictlyIncreasingHelper (rest listValues)))
                    (strictlyIncreasingHelper (rest listValues)))
                (strictlyIncreasingHelper (rest listValues)))))))