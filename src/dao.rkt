#lang racket
(require racket/file)
;(require compose-app)

(provide hashFromFileContent
         test_seeDiscardedLinesFromFileContent
         test_seeApprovedLinesFromFileContent
         fileToStringContent)

(define (hashFromFileContent
         reg_splitCodeAndChinese
         reg_chineseFirstOrLast
         reg_charsToRemove
         reg_splitFileString
         str_fileToStringContent)
  (hashWithCodeFirst
  (splitEachString reg_splitCodeAndChinese
  (filter  (lambda (str_each) (isAcceptableChinese reg_chineseFirstOrLast str_each))
  (cleanStringsBeforeHashmaping reg_charsToRemove
  (regexp-split  reg_splitFileString str_fileToStringContent ))))))

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

(define (test_seeDiscardedLinesFromFileContent
         reg_chineseFirstOrLast
         reg_charsToRemove
         reg_splitFileString
         str_fileContentToString)
  (filter  (lambda (str_each) (not (isAcceptableChinese reg_chineseFirstOrLast str_each)))
  (cleanStringsBeforeHashmaping reg_charsToRemove
  (regexp-split  reg_splitFileString str_fileContentToString ))))

(define (test_seeApprovedLinesFromFileContent
         reg_chineseFirstOrLast
         reg_charsToRemove
         reg_splitFileString
         str_fileContentToStringContent)
  (filter  (lambda (str_each) (isAcceptableChinese reg_chineseFirstOrLast str_each))
  (cleanStringsBeforeHashmaping reg_charsToRemove
  (regexp-split  reg_splitFileString  str_fileContentToStringContent ))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hjaelpe funktioner ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (getAllElems fun_firstOrSecond li_li_str_chineseAndCodes)
  (map (lambda (li_str_each) (fun_firstOrSecond li_str_each))
   li_li_str_chineseAndCodes))

;(define (secondElemsDublicates li_li_str_chineseAndCodes)
;  (let ([firstElems (getAllElems first li_li_str_chineseAndCodes)]
;         [secondElems (getAllElems second li_li_str_chineseAndCodes)])
;    (map
;     (lambda (str_each) (
;                         filter ))
;     firstElems)))



(define (fileToStringContent str_filepath) (file->string str_filepath))

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

 
(define cangjieA (hashFromFileContent
                  #px"[\\s]+"
                  #px"^[a-z0-9\\.,/;]+[\\s]+[\\S]+$"
                  "\uFEFF#"
                  #rx"\n"
                  (fileToStringContent "../inputFiles/testdoc")))
;(define cangjieDiscardLines (test_seeDiscardedLinesFromFileContent
;                             #px"^[a-z0-9\\.,/;]+[\\s]+[\\S]+$"
;                             "\uFEFF#"
;                             #rx"\n"
;                             (fileToStringContent "../inputFiles/CangJie_textCHR.txt")))

;(define cangjieApprovedLines (test_seeApprovedLinesFromFileContent
;                             #px"^[a-z0-9\\.,/;]+[\\s]+[\\S]+$"
;                             "\uFEFF#"
;                             #rx"\n"
;                             (fileToStringContent "../inputFiles/CangJie_textCHR.txt")))


;slut
