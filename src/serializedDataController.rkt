#lang racket

(require "deserializeStaticFiles.rkt")



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
  (let* ([char_chosenSystem (getFirstCapitalLetter str_commandCode  Str_inputSystemField)]
         [oldCode (removeCapitalLetters StrOldCmdField)]
         [newCode (removeCapitalLetters str_commandCode)])
    (if
      (and (< 0 (string-length newCode))
           (not (equal? oldCode newCode)))
      (getResultFromCode newCode char_chosenSystem)
      ('()))))

;*******************************************************************************
;*************************** funktioner til udregning
;******************************************************************************

(define (getResultFromCode str_code char_system)
  (let* ([li_resultsStrings (hash-ref cangjie5Code str_code)])
    (map
     (lambda (eachResultString)
       (list eachResultString
             (if (hash-has-key? heisigTrad eachResultString) (hash-ref heisigTrad eachResultString) (identity ""))
             (if (hash-has-key? heisigSimp eachResultString) (hash-ref heisigSimp eachResultString) (identity ""))
             (if (hash-has-key? cedictTrad eachResultString) (hash-ref cedictTrad eachResultString) (identity ""))
             (if (hash-has-key? cedictSimp eachResultString) (hash-ref cedictSimp eachResultString) (identity ""))))           
     li_resultsStrings)))


;(hash-ref array30Char "方")
;(hash-ref array30Code "yhs")
;(hash-ref cangjie5Char "方")
;(hash-ref cangjie5Code "yhs")


(+ 5 7)


;slut