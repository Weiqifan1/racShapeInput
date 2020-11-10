#lang racket
(require racket/file)
;(require compose-app)




(define (hashFromFile
         reg_splitCodeAndChinese
         reg_chineseFirstOrLast
         reg_charsToRemove
         reg_splitFileString
         str_filename)
  (hashWithCodeFirst
  (splitEachString reg_splitCodeAndChinese
  (filter  (lambda (str_each) (isAcceptableChinese reg_chineseFirstOrLast str_each))
  (cleanStringsBeforeHashmaping reg_charsToRemove
  (regexp-split  reg_splitFileString   
  (file->string str_filename) ))))))

;(define cangjieA (hashFromFile
;                  #px"[\\s]+"
;                  #px"^[a-z0-9\\.,/;]+[\\s]+[\\S]+$"
;                  "\uFEFF#"
;                  #rx"\n"
;                  "testdoc"))
;(define cangjieDiscardLines (test_seeDiscardedLinesFromFile
;                             #px"^[a-z0-9\\.,/;]+[\\s]+[\\S]+$"
;                            "\uFEFF#"
;                             #rx"\n"
;                             "testdoc"))

(define (test_seeDiscardedLinesFromFile
         reg_chineseFirstOrLast
         reg_charsToRemove
         reg_splitFileString
         str_filename)
  (filter  (lambda (str_each) (not (isAcceptableChinese reg_chineseFirstOrLast str_each)))
  (cleanStringsBeforeHashmaping reg_charsToRemove
  (regexp-split  reg_splitFileString   
  (file->string str_filename) ))))

(define (test_seeApprovedLinesFromFile
         reg_chineseFirstOrLast
         reg_charsToRemove
         reg_splitFileString
         str_filename)
  (filter  (lambda (str_each) (isAcceptableChinese reg_chineseFirstOrLast str_each))
  (cleanStringsBeforeHashmaping reg_charsToRemove
  (regexp-split  reg_splitFileString   
  (file->string str_filename) ))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hjaelpe funktioner ;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (removeUndesiredChars str_input str_badChars)
  (list->string (remove* (string->list str_badChars) (string->list str_input))))

(define (removeCharsFromList  str_badChars li_str_input )
  (map (lambda (str_each) (removeUndesiredChars str_each str_badChars)) li_str_input))

(define (cleanStringsBeforeHashmaping reg_charsToRemove li_str_input)
  (map string-downcase
   (removeCharsFromList reg_charsToRemove  li_str_input ))
  )

(define (isAcceptableChinese reg_chineseFirstOrLast str_input)
  (if (regexp-match? reg_chineseFirstOrLast str_input)
      #t #f))


(define (splitEachString reg_splitCodeAndChinese li_str_codeAndChinese)
  (map (lambda (str_each) (string-split str_each reg_splitCodeAndChinese))
       li_str_codeAndChinese))

(define (hashWithCodeFirst li_li_str_codeAndChinese)
  (for/hash ([li_str_each li_li_str_codeAndChinese])
    (if (regexp-match? #px"^[a-z0-9\\.,/;]+$" (first li_str_each))
        (values (first li_str_each) (second li_str_each))
        (values (second li_str_each) (first li_str_each)))))

 
(define cangjieA (hashFromFile
                  #px"[\\s]+"
                  #px"^[a-z0-9\\.,/;]+[\\s]+[\\S]+$"
                  "\uFEFF#"
                  #rx"\n"
                  "testdoc"))
(define cangjieDiscardLines (test_seeDiscardedLinesFromFile
                             #px"^[a-z0-9\\.,/;]+[\\s]+[\\S]+$"
                             "\uFEFF#"
                             #rx"\n"
                             "testdoc"))

(define cangjieApprovedLines (test_seeDiscardedLinesFromFile
                             #px"^[a-z0-9\\.,/;]+[\\s]+[\\S]+$"
                             "\uFEFF#"
                             #rx"\n"
                             "testdoc"))


;slut
