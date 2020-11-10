#lang racket
(require racket/file)

(require "dao.rkt")


;;;;;;;;;;;;;;;;;;;;;;strings med alle inputmetoderne ;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define cangjieTestStr (fileToStringContent "../inputFiles/testdoc"))

(define cangjieTest
  (hashFromFileContent   
                  #px"[\\s]+"
                  #px"^[a-z0-9\\.,/;]+[\\s]+[\\S]+$"
                  "\uFEFF#"
                  #rx"\n"
                  cangjieTestStr
                 
   ))




;(define (hashFromFile
;         reg_splitCodeAndChinese
;         reg_chineseFirstOrLast
;         reg_charsToRemove
;         reg_splitFileString
;         str_filename)
;  (hashWithCodeFirst
;  (splitEachString reg_splitCodeAndChinese
;  (filter  (lambda (str_each) (isAcceptableChinese reg_chineseFirstOrLast str_each))
;  (cleanStringsBeforeHashmaping reg_charsToRemove
;  (regexp-split  reg_splitFileString   
;  (file->string str_filename) ))))))

;(define cangjieA (hashFromFile
;                  #px"[\\s]+"
;                  #px"^[a-z0-9\\.,/;]+[\\s]+[\\S]+$"
;                  "\uFEFF#"
;                  #rx"\n"
;                  "testdoc"))







;slut
