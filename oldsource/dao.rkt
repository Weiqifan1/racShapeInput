#lang racket
(require racket/file)
;(require compose-app)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; funktionerne er klar til export ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; der er 4 funktioner:  


(provide inputHashGen
         cedictHashGen
         tzaiHashGen
         jundaHashGen
)

;;; 2020/11/03 create unicode function, contracts, xml
;unicode>
(define (charsToUnicode str_inputString)
  (if (and (string? str_inputString)
           (< 0 (string-length str_inputString)))
      (map
       (lambda (char_eachChar) (char->integer (string-ref char_eachChar 0)))  
           (string-split str_inputString #rx"(?<=.)(?=.)"))
      '()))
;(char->integer (string-ref "Ʃ" 0))
;(char->integer (string-ref "𠁆" 0))

;contracts


;XML
;jeg bruger ikke xml. jeg bruger bare almindelige funktioner til at gemme datastructurerne i en file:

;(write-to-file testPrintHash "../dataStructures/cedictHash")
;;;;;(files laves succefuldt).
;(define testReadCode (read (open-input-string (file->string "../dataStructures/cedictHash"))))
;;;;test:  (hash-ref testReadCode "B")  => "2" (det virker fint!!!)


;contracts
;;;det er g(et saa godt at jeg gaa i seng. jeg venter med contracts til i morgen))



;;;


(define (inputHashGen
         fun_whichToCompare
         fun_whichToAppend
         reg_splitCodeAndChinese
         reg_chineseFirstOrLast
         reg_charsToRemove
         reg_splitFileString
         str_fileToStringContent)
  (hashWithCodeFirst
  (replaceCounterpartWithList fun_whichToCompare fun_whichToAppend 
  (splitEachString reg_splitCodeAndChinese
  (filter  (lambda (str_each) (isAcceptableChinese reg_chineseFirstOrLast str_each))
  (cleanStringsBeforeHashmaping reg_charsToRemove
  (regexp-split  reg_splitFileString str_fileToStringContent )))))))


(define cedict (file->string "../inputFiles/testcedict"))

;(substring cedict 0 1000)


(define (cedictHashGen fun_chooseKey str_cedictFileContent)
  (hashFromCedict fun_chooseKey
  (filter (lambda (li_str_each) (< 3 (length li_str_each)))
  (map (lambda (str_eachLine) (regexp-split #rx"###" str_eachLine)) 
  (regexp-split #rx"\n" str_cedictFileContent)))))


(define tzai (file->string "../inputFiles/Tzai2006.txt"))

(define (tzaiHashGen int_start int_end str_cedictFileContent)
  (let ([li_str_tzaiCharNum (tzaiNumGen int_start int_end str_cedictFileContent)])
    (for/hash ([str_each li_str_tzaiCharNum])
      (values (first (string-split str_each " ")) (second (string-split str_each)) ))))

(define junda (file->string "../inputFiles/Junda2005"))

(define (jundaHashGen fun_keyindex fun_valindex str_jundafilecontent)
  (let ([li_li_str_numbersandchars
         (jundaContentToNestedList #px"^[0-9]+[\\s]+[\\S]+[\\s]+.*$" str_jundafilecontent)])
    (for/hash ([li_str_eachline li_li_str_numbersandchars])
      (values (fun_keyindex li_str_eachline) (fun_valindex li_str_eachline)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; her er eksempler paa funktionskaldene ;;;;;;;;;;;;;;;
;; der er tilsvarende eksempler i bunden af filen

;hashmap of cangjie codes to List of cangjie Chars
;(define cangjieA (inputHashGen
;                  first
;                  second
;                  #px"[\\s]+"
;                  #px"^[a-z0-9\\.,/;]+[\\s]+[\\S]+$"
;                  "\uFEFF#"
;                  #rx"\n"
;                  (fileToStringContent "../inputFiles/testdoc")))

;hashmap of cangjieChars to list of cangjie codes
;(define cangjieB (inputHashGen
;                  second
;                  first
;                  #px"[\\s]+"
;                  #px"^[a-z0-9\\.,/;]+[\\s]+[\\S]+$"
;                  "\uFEFF#"
;                  #rx"\n"
;                  (fileToStringContent "../inputFiles/testdoc")))


;(define cedictHashTradKey
;  (cedictHashGen first cedict))

;(define cedictHashSimpKey
;  (cedictHashGen second cedict))

;(define tzaihash (tzaiHashGen 0 1 tzai))

;(define jundahash (jundaHashGen second first junda))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; funktioner til at hente cedict, junda og tzai ;;;;;;


(define (hashFromCedict fun_chooseKey li_li_str_codeAndChinese)
  (for/hash ([li_str_each li_li_str_codeAndChinese])
    (values (fun_chooseKey li_str_each) li_str_each)))


(define (stringWithlineNumber str_each vec_str_allLines)
  (string-append str_each " " (vector-member str_each vec_str_allLines)))

(define (tzaiTrimmed int_start int_end str_cedictFileContent)
  ;(hashFromCedict fun_chooseKey
  ;(filter (lambda (li_str_each) (< 3 (length li_str_each)))
  ;(map (lambda (str_eachLine) (regexp-split #rx"###" str_eachLine))
  ;(let ([count 1]) ()
  (map (lambda (str_each) (substring str_each int_start int_end))
  (regexp-split #rx"\n" (substring str_cedictFileContent 1))))

;(define tzailist (tzaiTrimmed 0 1 tzai))

(define (tzaiNumGen int_start int_end str_cedictFileContent)
  (let* ([tzailist (tzaiTrimmed int_start int_end str_cedictFileContent)]
         [tzaivec (list->vector tzailist)])
    (map (lambda (str_each) (string-append str_each " " (format "~v" (+ 1 (vector-member str_each tzaivec)))))
         tzailist)))

;(define tzainum (tzaiNumGen 0 1 tzai))



;;;;;;;;;;;2020-11-02 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;det gik fint med tzai. Nu skal jeg pr've med junda


(define (jundaContentToNestedList reg_matchingRegex str_jundaFileContent)
  (map (lambda (str_eachline) (string-split str_eachline #px"[\t]+"))
  (filter (lambda (str_eachline) (regexp-match? reg_matchingRegex str_eachline))  
  (string-split str_jundaFileContent #rx"\n"))))

 ;(if (regexp-match? #px"^[a-z0-9\\.,/;]+$" (first li_str_each))


(define filterjunda (jundaContentToNestedList  #px"^[0-9]+[\\s]+[\\S]+[\\s]+.*$" junda))
;#px"^[0-9]+[\\s]+\\S[\\s]+.*$"


;(regexp-match? #px"^[0-9]+[\\s]+[\\S]+[\\s]+.*$" "12039\t靘\t1\t99.99999922736\tqing1\t")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; test Files ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(define (appendIfPairMatch
         str_matchToPair
         fun_whichToCompare
         fun_whichToAppend
         li_str_codeKinPair
         li_str_dublicates)
  (cond
    [(equal? str_matchToPair (fun_whichToCompare li_str_codeKinPair))
      (append  li_str_dublicates (fun_whichToAppend li_str_codeKinPair) )]
      ))
;(appendIfPairMatch "aabb" first second '("aabb" "hiHao") '("i have nihao"))

(define (filterCounterpartMatches
         str_matchToPair
         fun_whichToCompare
         li_li_str_codeKinPair)
  (filter (lambda (li_str_each)
            (equal? str_matchToPair (fun_whichToCompare li_str_each)))
   li_li_str_codeKinPair))

(define (combineRemainingCounterparts
         fun_whichToAppend
         li_li_str_codeKinPair)
  (map (lambda (li_str_each)
         (fun_whichToAppend li_str_each))
       li_li_str_codeKinPair))

(define (filterAndCombineCounterparts
         str_matchToPair
         fun_whichToCompare
         fun_whichToAppend       
         li_li_str_codeAndChinese)
  (combineRemainingCounterparts
   fun_whichToAppend
  (filterCounterpartMatches
   str_matchToPair
   fun_whichToCompare
   li_li_str_codeAndChinese)))

(define (replaceCounterpartWithList
         fun_whichToCompare
         fun_whichToAppend       
         li_li_str_codeAndChinese)
  (map (lambda (li_str_each)
         (list (fun_whichToCompare li_str_each)
               (filterAndCombineCounterparts
                (fun_whichToCompare li_str_each)
                fun_whichToCompare
                fun_whichToAppend
                li_li_str_codeAndChinese)))
   li_li_str_codeAndChinese))




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
    ;(if (regexp-match? #px"^[a-z0-9\\.,/;]+$" (first li_str_each))
        (values (first li_str_each) (second li_str_each))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eksempler paa funktionerne ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;hashmap of cangjie codes to List of cangjie Chars
;(define cangjieA (inputHashGen
;                  first
;                  second
;                  #px"[\\s]+"
;                  #px"^[a-z0-9\\.,/;]+[\\s]+[\\S]+$"
;                  "\uFEFF#"
;                  #rx"\n"
;                  (fileToStringContent "../inputFiles/testdoc")))

;hashmap of cangjieChars to list of cangjie codes
;(define cangjieB (inputHashGen
;                  second
;                  first
;                  #px"[\\s]+"
;                  #px"^[a-z0-9\\.,/;]+[\\s]+[\\S]+$"
;                  "\uFEFF#"
;                  #rx"\n"
;                  (fileToStringContent "../inputFiles/testdoc")))


;(define cedictHashTradKey
;  (cedictHashGen first cedict))

;(define cedictHashSimpKey
;  (cedictHashGen second cedict))

;(define tzaihash (tzaiHashGen 0 1 tzai))

;(define jundahash (jundaHashGen second first junda))





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
