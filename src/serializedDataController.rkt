#lang racket

(require "deserializeStaticFiles.rkt")

(provide convertCommandCodeToList)
;(convertCommandCodeToList StrCommandCode Str_inputSystemField)
; (convertCommandCodeToList "hqi" "A")

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

(define (nestedListOfUnicodeAndStrings li_listOfCharsFromInputSystem str_cleanCommandCode)
  (if (equal? li_listOfCharsFromInputSystem '())
      '()
      (map
       (lambda (str_eachChineseString)
         (list (getComparisonNumberFromChineseString str_eachChineseString)
               str_eachChineseString
               str_cleanCommandCode
               ;;call here the methods needed to get information about the chineseStrings to write
               (getHeisigInfo str_eachChineseString)
               (codeToCedictResultString str_eachChineseString cedictTrad cedictSimp)
               
               ))
       li_listOfCharsFromInputSystem)))

(define (sortNestedList li_li_numberAndChineseString)
  (if (equal? li_li_numberAndChineseString '())
      '()
      (map
       (lambda (li_numberAndString)
         (filter
          (lambda (numberOrString) (string? numberOrString))
          li_numberAndString))
       (sort li_li_numberAndChineseString #:key first <))))


(define (convertCommandCodeToList str_commandCode Str_inputSystemField)
  (let* ([str_updatedInputMethodLetter (getFirstCapitalLetter  str_commandCode Str_inputSystemField)]
         [str_cleanCommandCode (removeCapitalLetters str_commandCode)]
         [listOfCharsFromInputSystem (getListOfInputSystemStrings str_cleanCommandCode str_updatedInputMethodLetter)]
         [nestedList (nestedListOfUnicodeAndStrings listOfCharsFromInputSystem str_cleanCommandCode)]
         [sortedList (sortNestedList nestedList)])
    (identity sortedList)))

;;******************************************************************************************
;;************ methods needed to get information about a chineseString beyond the string itself
;;******************************************************************************************

; get heisig info from shinese string
(define (getBasicHeisigInfo str_chineseString str_heisig map_heisig)
  (if (hash-has-key? map_heisig str_chineseString)
      (string-append str_heisig
                     ":"
                     (number->string (hash-ref (hash-ref map_heisig str_chineseString) 'heisignumber))
                     " "
                     (hash-ref (hash-ref map_heisig str_chineseString) 'heisigmeaning))
      (string-append str_heisig ":none")))

(define (getHeisigInfo str_chineseString)
  (let* ([heisigtrad (getBasicHeisigInfo str_chineseString "HeisigTrad" heisigTrad)]
         [heisigsimp (getBasicHeisigInfo str_chineseString "HeisigSimp" heisigSimp)])
    (string-append heisigtrad " " heisigsimp)))

;********* get cedict value:
;(hash-ref cedictTrad "方")
;(("中" "中" "[Zhong1][zhong1][zhong4]" "/China/Chinese/surname Zhong/ [zhong1]/within/among/in/middle/center/while (doing sth)/during/(diale..."))
;  '(("丨" "丨" "[gun3][shu4]" "/radical in Chinese characters (Kangxi radical 2)/ [shu4]/see 豎筆|竖笔[shu4 bi3]/"))

;  #hash((cedictinfo . (("中" "中" "[Zhong1][zhong1][zhong4]" "/China/Chinese/surname Zhong/ [zhong1]/within/among/in/middle/center/while (doing sth)/during/(dialect) OK/all right/ [zhong4]/to hit (the mark)/to be hit by/to suffer/to win (a prize, a lottery)/"))) (comparison . (11)) (corpussize . (171894734)) (corpustype . ("traditional")) (frequencyrank . (11)) (key . "中") (occurrences . (1322363)) (unicode . (20013)))

;  #hash((cedictinfo . (("丨" "丨" "[gun3][shu4]" "/radical in Chinese characters (Kangxi radical 2)/ [shu4]/see 豎筆|竖笔[shu4 bi3]/"))) (comparison . (200080)) (corpussize . (0)) (corpustype . (0)) (frequencyrank . (0)) (key . "丨") (occurrences . (0)) (unicode . (20008)))

(define (appendStringsFromNestedList li_li_string);(getResultFromCode "l" "")
  (if (< 1 (length li_li_string)) 
      (apply string-append
         (map (lambda (eachList) (apply string-append eachList)) li_li_string))
      (first (map (lambda (eachList) (apply string-append eachList)) li_li_string))))

(define (cedictResultOfSingleCharToString map_cedictResult)
  (list
    (if (equal? 0 (first (hash-ref map_cedictResult 'corpustype)))
        ""
        (first (hash-ref map_cedictResult 'corpustype)))
    " freq:"
    (if (equal? 0 (first (hash-ref map_cedictResult 'frequencyrank)))
        ""
        (number->string (first (hash-ref map_cedictResult 'frequencyrank))))
    " "
   (appendStringsFromNestedList
    (hash-ref map_cedictResult 'cedictinfo))))

(define (cedictResultOfMultipleCharToString map_cedictResult)
  (list
   ;(first (hash-ref map_cedictResult 'corpustype))
   ;" "
   (appendStringsFromNestedList
    (hash-ref map_cedictResult 'cedictinfo))))

(define (codeToCedictResultString str_chineseStr map_cedictTrad map_cedictSimp)
  (apply string-append 
  (if (hash-has-key? map_cedictTrad str_chineseStr)           
      (let ([map_tradEntry (hash-ref map_cedictTrad str_chineseStr)])
        (if (equal? 1 (string-length (hash-ref map_tradEntry 'key)))
            (cedictResultOfSingleCharToString map_tradEntry)
            (cedictResultOfMultipleCharToString map_tradEntry)))
      (if (hash-has-key? map_cedictSimp str_chineseStr)
          (let ([map_simpEntry (hash-ref map_cedictSimp str_chineseStr)])
            (if (equal? 1 (string-length (hash-ref map_simpEntry 'key)))
                (cedictResultOfSingleCharToString map_simpEntry)
                (cedictResultOfMultipleCharToString map_simpEntry)))
          (list "no cedict entry")))))


;******


;slut