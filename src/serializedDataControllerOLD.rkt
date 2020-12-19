#lang racket


#;(list (list "牛"
   "hq"
   791
   "HeisigTrad:235 cow HeisigSimp:252 cow"
   "traditional freq:791 牛牛[Niu2][niu2]/surname Niu/ [niu2]/ox/cow/bull/CL:條|条[tiao2],頭|头[tou2]/newton (abbr. for 牛頓|牛顿[niu2 dun4])/(slang) awesome/")
  (list "牜" "hq" 292760 "HeisigTrad:none HeisigSimp:none" "no cedict entry")
  (list "𥬄" "hq" 1543720 "HeisigTrad:none HeisigSimp:none" "no cedict entry")
  (list "\U000FB629" "hq" 10296730 "HeisigTrad:none HeisigSimp:none" "no cedict entry"))


#;(list (list (string-append  "1a " "abc")
              (string-append  "1b " "abc"))
        (list (string-append  "2a " "abc")
              (string-append  "2b " "abc"))
        )

(require "deserializeStaticFiles.rkt")

(provide convertCommandCodeToList)

;;method example:
;(convertCommandCodeToList "hq" "a" "A")
;'(("牛"
;   "hq"
;   "HeisigTrad:235 cow HeisigSimp:252 cow"
;   "traditional freq:791 牛牛[Niu2][niu2]/surname Niu/ [niu2]/ox/cow/bull/CL:條|条[tiao2],頭|头[tou2]/newton (abbr. for 牛頓|牛顿[niu2 dun4])/(slang) awesome/")
;  ("牜" "hq" "HeisigTrad:none HeisigSimp:none" "no cedict entry")
;  ("𥬄" "hq" "HeisigTrad:none HeisigSimp:none" "no cedict entry")
;  ("\U000FB629" "hq" "HeisigTrad:none HeisigSimp:none" "no cedict entry"))

;returns char
(define (getFirstCapitalLetter StrCommandCode Str_inputSystemField)
  (if (and (< 0 (string-length StrCommandCode))
           (< 64 (char->integer (string-ref StrCommandCode 0))) ;A
           (> 91 (char->integer (string-ref StrCommandCode 0)))) ;Z
      (string-ref StrCommandCode 0)
      (string-ref Str_inputSystemField 0)))

(define (removeCapitalLetters StrCommandCode)
  (let ([inputList (string->list StrCommandCode)])
    (list->string
    (filter
     (lambda (eachChar)
       (not (char-upper-case? eachChar)))
     inputList))))


;(define (getResultOfCommandCode StrCommandCode StrOldCmdField Str_inputSystemField currentClass)
(define (convertCommandCodeToList str_commandCode StrOldCmdField Str_inputSystemField)
  (map (lambda (li_stringsAndInts)
         (filter (lambda (stringOrInt) (string? stringOrInt)) li_stringsAndInts))
  (sort 
  (let* ([char_chosenSystem (getFirstCapitalLetter str_commandCode  Str_inputSystemField)]
         [oldCode (removeCapitalLetters StrOldCmdField)]
         [newCode (removeCapitalLetters str_commandCode)]
         [results (getResultFromCode newCode char_chosenSystem)])
    (if
      (and (< 0 (string-length newCode))
           (not (equal? oldCode newCode)))
      (identity results)
      ('( (0 "" "") (0 "" "")))))
  #:key first <)
))

;*******************************************************************************
;*************************** funktioner til udregning
;******************************************************************************

;(define (getResultFromCode str_code char_system)
;  (let* ([li_resultsStrings (hash-ref cangjie5Code str_code)])
;    (map
;     (lambda (eachResultString)
;       (list eachResultString
;             (if (hash-has-key? heisigTrad eachResultString) (hash-ref heisigTrad eachResultString) (identity ""))
;             (if (hash-has-key? heisigSimp eachResultString) (hash-ref heisigSimp eachResultString) (identity ""))
;             (if (hash-has-key? cedictTrad eachResultString) (hash-ref cedictTrad eachResultString) (identity ""))
;             (if (hash-has-key? cedictSimp eachResultString) (hash-ref cedictSimp eachResultString) (identity ""))))           
;     li_resultsStrings)))

(define (getResultFromCode str_code char_system)
  (let* ([li_resultsStrings (hash-ref cangjie5Code str_code)])
    (map
     (lambda (eachResultString)
       (list (getComparisonNumberFromChineseString eachResultString)
             eachResultString
             str_code            
             (getHeisigInfo eachResultString)
             (codeToCedictResultString eachResultString cedictTrad cedictSimp)
             ))           
     li_resultsStrings)))

;********* get cedict value:
;(hash-ref cedictTrad "方")
;(("中" "中" "[Zhong1][zhong1][zhong4]" "/China/Chinese/surname Zhong/ [zhong1]/within/among/in/middle/center/while (doing sth)/during/(diale..."))
(define (appendStringsFromNestedList li_li_string);(getResultFromCode "l" "")
  ;(if (< 1 (length li_li_string)) 
      (apply string-append
         (map (lambda (eachList) (apply string-append eachList)) li_li_string)))
      ;(map (lambda (eachList) (apply string-append eachList)) li_li_string)))

(define (cedictResultOfSingleCharToString map_cedictResult)
  (list
    (first (hash-ref map_cedictResult 'corpustype))
    " freq:"
    (number->string (first (hash-ref map_cedictResult 'frequencyrank)))
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

;********* get comparison number

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

;(number->string 345)

; get heisig info from shinese string

;eks: (getResultFromCode "yhs" #\A)
;skriv nu en funktion der ogsaa tager hensyn til at et tegn kan findes i baade
;traditionel og simplificeret heisig

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

;;(define (getHeisigInfo str_chineseString)
;  (if (hash-has-key? heisigTrad str_chineseString)
;      (string-append "heisigTrad:"
;                     (number->string (hash-ref (hash-ref heisigTrad str_chineseString) 'heisignumber))
;                     (hash-ref (hash-ref heisigTrad str_chineseString) 'heisigmeaning))
;      (if (hash-has-key? heisigSimp str_chineseString)
;          (string-append "heisigSimp:"
;                     (number->string (hash-ref (hash-ref heisigSimp str_chineseString) 'heisignumber))
;                     (hash-ref (hash-ref heisigSimp str_chineseString) 'heisigmeaning))
;          (identity "notHeisigCharacter"))))

;(hash-ref array30Char "方")
;(hash-ref array30Code "yhs")
;(hash-ref cangjie5Char "方")
;(hash-ref cangjie5Code "yhs")


;(+ 5 7)


;slut