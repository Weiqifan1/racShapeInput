#lang racket

(require "deserializeStaticFiles.rkt")

(provide convertCommandCodeToList)

;; handle capital letters
(define (removeCapitalLetters StrCommandCode)
  (let ([inputList (string->list StrCommandCode)])
    (list->string
    (filter
     (lambda (eachChar)
       (not (char-upper-case? eachChar)))
     inputList))))

(define (getFirstCapitalLetter StrCommandCode Str_inputSystemField)
  (if (and (< 0 (string-length StrCommandCode))
           (< 64 (char->integer (string-ref StrCommandCode 0))) ;64 == A-1
           (> 91 (char->integer (string-ref StrCommandCode 0))));91 == Z+1
      (string-ref StrCommandCode 0)
      (string-ref Str_inputSystemField 0)))

;;handle unicode and comparison numbers
(define (largestUnicodeNumber str_inputString)
  (let ([li_char (string->list str_inputString)])
    (if (< 0 (length li_char))
        (last
          (sort
          (map (lambda (eachChar) (char->integer eachChar)) li_char) <))
        (identity 0))))

(define (comparisonNumOfUnknownString str_input)
  (* 10
  (largestUnicodeNumber str_input)))

(define (getComparisonNumberFromChineseString str_chineseString)
  (if (hash-has-key? cedictTrad str_chineseString)
       (first (hash-ref (hash-ref cedictTrad str_chineseString) 'comparison))
       (if (hash-has-key? cedictSimp str_chineseString)
           (first (hash-ref (hash-ref cedictSimp str_chineseString) 'comparison))
           (comparisonNumOfUnknownString str_chineseString))))

;this method always return cangjie character results
(define (getListOfInputSystemStrings str_commandCode Str_inputSystemField)
  (if (and (< 0 (string-length str_commandCode) )
           (hash-has-key? cangjie5Code str_commandCode))
      (hash-ref cangjie5Code str_commandCode)
      '()))

(define (nestedListOfUnicodeAndStrings li_listOfCharsFromInputSystem)
  (if (equal? li_listOfCharsFromInputSystem '())
      '()
      (map
       (lambda (str_eachChineseString)
         (list (getComparisonNumberFromChineseString str_eachChineseString)
               str_eachChineseString))
       li_listOfCharsFromInputSystem)))

(define (sortNestedList li_numberAndChineseString)
  (if (equal? li_numberAndChineseString '())
      '()
      (map
       (lambda (li_numberAndString)
         (filter
          (lambda (numberOrString) (string? numberOrString))
          li_numberAndString))
       (sort li_numberAndChineseString #:key first <))))


(define (convertCommandCodeToList str_commandCode Str_inputSystemField)
  (let* ([str_updatedInputMethodLetter (getFirstCapitalLetter  str_commandCode Str_inputSystemField)]
         [str_cleanCommandCode (removeCapitalLetters str_commandCode)]
         [listOfCharsFromInputSystem (getListOfInputSystemStrings str_cleanCommandCode str_updatedInputMethodLetter)]
         [nestedList (nestedListOfUnicodeAndStrings listOfCharsFromInputSystem)]
         [sortedList (sortNestedList nestedList)])
    (identity sortedList)))


;******


;slut