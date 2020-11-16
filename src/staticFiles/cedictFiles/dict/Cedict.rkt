#lang racket

(require "Junda.rkt" "Tzai.rkt")
;(require racket/serialize) ;used for serializing data - look at the bottom of the file
(require racket/fasl)

"start of cedict calculation - a message will be written when the calculation is done"

;(length (hash-values tzaihash))
;(length (hash-values jundahash))

(define (cedictHashGen fun_chooseKey str_cedictFileContent)
  (hashFromCedict fun_chooseKey
  (filter (lambda (li_str_each) (< 3 (length li_str_each)))
  (map (lambda (str_eachLine) (regexp-split #rx"###" str_eachLine)) 
  (regexp-split #rx"\n" str_cedictFileContent)))))

(define (hashFromCedict fun_chooseKey li_li_str_codeAndChinese)
  (for/hash ([li_str_each li_li_str_codeAndChinese])
    (values (fun_chooseKey li_str_each) li_str_each)))

;(define cedictHashTradKey
;  (cedictHashGen first cedict))

;(define cedictHashSimpKey
;  (cedictHashGen second cedict))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; functions to create nested Cedict lists ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;([set_cedictKeys (list->set
 ;                        (map
 ;                         (lambda (li_eachStrAndHash) (first li_eachStrAndHash))
 ;                         li_li_strAndHash))]
 ;       [li_freqSystemkeys (hash-keys ha_freqSystem)])

;create a nested list with all the cedict lines


(define (addMissingSystemCharsToCedict fun_elementToUseAsKey ha_frequencySystem li_li_str_eachCedictLine)
  (let* ([set_cedictKeys
          (list->set
           (map
           (lambda (li_eachlist) (fun_elementToUseAsKey li_eachlist))
           li_li_str_eachCedictLine))]
         [li_systemKeys (hash-keys ha_frequencySystem)]
         [li_missingKeys
          (filter
           (lambda (str_eachChar)
             (not (set-member? set_cedictKeys str_eachChar)))
           li_systemKeys)])
    (append li_li_str_eachCedictLine
    (map
     (lambda (str_eachMissingChar)              
          (list str_eachMissingChar
                str_eachMissingChar
                "[]"
                "/NOTE: the char. is not in cedict. The trad. simp. pair might be wrong"))
     li_missingKeys))))

(define (cedictNestedList str_cedictFileContent)
  ;;(map (lambda (li_listOfStrings) (list (fun_chooseKey li_listOfStrings) li_listOfStrings))
  (filter (lambda (li_str_each) (< 3 (length li_str_each)))
  (map (lambda (str_eachLine) (regexp-split #rx"###" str_eachLine))
       (regexp-split #rx"\n" str_cedictFileContent))))

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

(define (combineRemainingCounterparts
         fun_whichToAppend
         li_li_str_codeKinPair)
  (map (lambda (li_str_each)
         (fun_whichToAppend li_str_each))
       li_li_str_codeKinPair))

(define (filterCounterpartMatches
         str_matchToPair
         fun_whichToCompare
         li_li_str_codeKinPair)
  (filter (lambda (li_str_each)
            (equal? str_matchToPair (fun_whichToCompare li_str_each)))
   li_li_str_codeKinPair))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; functions to add unicode and serialnumbers to cedict info;;;;;;;;;;;;;;;;;;;;;;;;;;;

;write a function that takes a string and return a list of junda or tzai serialnumbers
(define (charsToFrequency hash_frequencySystem str_inputString)
  (if (and (string? str_inputString)
           (< 0 (string-length str_inputString)))
      (map
       (lambda (char_eachChar)
         (if (hash-has-key? hash_frequencySystem (string char_eachChar))
             (hash-ref hash_frequencySystem (string char_eachChar))
             0))  
           (string->list str_inputString))
      '()))

;(charsToFrequency jundahash "ab𠁆Ʃ子")
;(charsToFrequency tzaihash "ab𠁆Ʃ子")
;(hash-has-key? tzaihash "子")

;write a function that takes a string and return a list of unicode codepoints

(define (charsToUnicode str_inputString)
  (if (and (string? str_inputString)
           (< 0 (string-length str_inputString)))
      (map
       (lambda (char_eachChar) (char->integer (string-ref (string char_eachChar) 0)))  
           (string->list str_inputString))
      '()))
;string-split str_inputString #rx"(?<=.)(?=.)"
;(char->integer (string-ref "Ʃ" 0))
;(char->integer (string-ref "𠁆" 0))
;(charsToUnicode "𠁆")
;(charsToUnicode "ab𠁆Ʃ")

;write a function that takes a string and return a list of either serialnumbers or codepoints X 10.
(define (charsToComparisonNums hash_frequencySystem str_inputString)
  (if (and (string? str_inputString)
           (< 0 (string-length str_inputString))) 
      ;(filter (lambda (int_eachInt) (< 0 int_eachInt)) 
      (map
       (lambda (char_eachChar)
         (charToComparisonNum hash_frequencySystem char_eachChar))
           (string->list str_inputString))
      '()))

(define (charToComparisonNum hash_frequencySystem char_input)
  (let* ([isChinese (< 11900 (char->integer char_input))]
         [isInFrecSystem (hash-has-key? hash_frequencySystem (string char_input))])
   
    (if isChinese
        (if isInFrecSystem
            (hash-ref (hash-ref hash_frequencySystem (string char_input)) 'frequencyrank)
            (* 10 (char->integer char_input)))
        0)))

;(charsToComparisonNums jundahash "𠁆")
;(charsToComparisonNums jundahash "ab𠁆Ʃ子")
;(charsToComparisonNums tzaihash "𠁆")
;(charsToComparisonNums tzaihash "ab𠁆Ʃ子")

(define (charsToFreqSystemNums hash_frequencySystem str_inputString)
  (if (and (string? str_inputString)
           (< 0 (string-length str_inputString))) 
      ;(filter (lambda (int_eachInt) (< 0 int_eachInt)) 
      (map
       (lambda (char_eachChar)
         (charToFreqSystemNum hash_frequencySystem char_eachChar))
           (string->list str_inputString))
      '()))
  

(define (charToFreqSystemNum hash_frequencySystem char_input)
  (let* ([isChinese (< 11900 (char->integer char_input))]
         [isInFrecSystem (hash-has-key? hash_frequencySystem (string char_input))])
   
    (if isChinese
        (if isInFrecSystem
            (hash-ref (hash-ref hash_frequencySystem (string char_input)) 'frequencyrank)
            0)
        0)))
;(charsToFreqSystemNums jundahash "ab𠁆Ʃ子")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;; write a function that retrieves the occurrences of a character ;;;;;;
(define (getValueFromNestedHash ha_ha_FreqSystem sym_systemSymbolKey str_lineKey)
  (map (lambda
           (char_eachStr)
      (if (hash-has-key? ha_ha_FreqSystem (string char_eachStr))
          (hash-ref (hash-ref ha_ha_FreqSystem (string char_eachStr)) sym_systemSymbolKey)
          0)) (string->list str_lineKey)) )

;; create a functions that creates a set from all keys in a frequencysystem
(define (keysToSet ha_freqsystem)
  (list->set
  (hash-keys ha_freqsystem)))

;create a function that adds lists of unicode, jundaCount and tzaiCount to the cedict nested list  
(define (nestedCedictToListHashMap hash_frequencySystem li_li_cedict)
  (map
   (lambda (li_eachLine)
     (let* ([lineKey (first li_eachLine)]
            [lineValue (second li_eachLine)])
       (list lineKey
             (hash 'key lineKey
                   'cedictinfo lineValue
                   'unicode (charsToUnicode lineKey)
                   'comparison (charsToComparisonNums hash_frequencySystem lineKey)
                   'frequencyrank (charsToFreqSystemNums hash_frequencySystem lineKey)
                   'occurrences (getValueFromNestedHash hash_frequencySystem 'occurrences lineKey)
                   'corpussize (getValueFromNestedHash hash_frequencySystem 'corpussize lineKey)
                   'corpustype (getValueFromNestedHash hash_frequencySystem 'corpustype lineKey)
                   ))))
   li_li_cedict))

(define (addMissingfreqSystemEntries ha_freqSystem li_li_strAndHash)
  (let* ([set_cedictKeys (list->set
                         (map
                          (lambda (li_eachStrAndHash) (first li_eachStrAndHash))
                          li_li_strAndHash))]
        [li_freqSystemkeys (hash-keys ha_freqSystem)])
    (map
     (lambda (lineKey)
         (append li_li_strAndHash
                 (list lineKey
                       (hash 'key lineKey
                             'cedictinfo '()
                             'unicode (charsToUnicode lineKey)
                             'comparison (charsToComparisonNums ha_freqSystem lineKey)
                             'frequencyrank  (charsToFreqSystemNums ha_freqSystem lineKey)
                             'occurrences  (getValueFromNestedHash ha_freqSystem 'occurrences lineKey)
                             'corpussize   (getValueFromNestedHash ha_freqSystem 'corpussize lineKey)
                             'corpustype  (getValueFromNestedHash ha_freqSystem 'corpustype lineKey)
                             ))))
    (filter
     (lambda (str_eachFreqsysKey)
       (not (set-member? set_cedictKeys str_eachFreqsysKey)))
     li_freqSystemkeys))))

(define (listOfCedictStrAndHashToHash li_strAndHash_cedict)
  (for/hash ([eachList li_strAndHash_cedict])
    (values (first eachList) (first eachList))))

(define (listOfCedictStrAndHashToHash2 li_strAndHash_cedict)
  (for/hash ([liOfCharAndHash li_strAndHash_cedict])
  (values (first liOfCharAndHash) (second liOfCharAndHash))))
  
  ;(hash? (second (first li_strAndHash_cedict))))

  ;(for/hash ([eachList li_strAndHash_cedict])
   ; (values (first eachList) (first eachList))))

(define (generateCedictHashFromSource
         hash_frequencySystem
         fun_whichToCompare
         fun_whichToAppend       
         str_cedictText)
  (listOfCedictStrAndHashToHash
  ;(addMissingfreqSystemEntries hash_frequencySystem 
  (nestedCedictToListHashMap hash_frequencySystem
  (replaceCounterpartWithList fun_whichToCompare fun_whichToAppend
  (addMissingSystemCharsToCedict fun_whichToCompare hash_frequencySystem
  (cedictNestedList str_cedictText))))))

(define (generateCedictHashFromSource2
         hash_frequencySystem
         fun_whichToCompare
         fun_whichToAppend       
         str_cedictText)
  (listOfCedictStrAndHashToHash2
  (nestedCedictToListHashMap hash_frequencySystem
  (replaceCounterpartWithList fun_whichToCompare fun_whichToAppend
  (addMissingSystemCharsToCedict fun_whichToCompare hash_frequencySystem
  (cedictNestedList str_cedictText))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; funktioner til at skrive cedict til filer ;;;;;;;;;;;;;;;

(define cedict (file->string "../cedictSource/cedictImproved.txt"))
(length (cedictNestedList cedict))
;(define cedict (file->string "../cedictSource/testcedict"))

;(define ha2b (generateCedictHashFromSource2 jundahash second identity cedict))
;(define ha3c (generateCedictHashFromSource2 tzaihash first identity cedict))

;"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2"


(+ 1 0)
;(call-with-output-file "../cedictMaps/cedictTradSerial.rktd"
;  (lambda (out) (s-exp->fasl ha3c out))
;  #:exists 'replace)
;(call-with-output-file "../cedictMaps/cedictSimpSerial.rktd"
;  (lambda (out) (s-exp->fasl ha2b out))
;  #:exists 'replace)

;(call-with-output-file "../cedictMaps/cedictTradDat.dat"
;  (lambda (out) (s-exp->fasl ha3c out))
;  #:exists 'replace)
;(call-with-output-file "../cedictMaps/cedictSimpDat.dat"
;  (lambda (out) (s-exp->fasl ha2b out))
;  #:exists 'replace)

(define (load-data path)
  (call-with-input-file path fasl->s-exp))

;(define readTrad (load-data "../cedictMaps/cedictTradSerial.rktd"))
;(define readSimp (load-data "../cedictMaps/cedictSimpSerial.rktd"))

;(define readTrad2 (load-data "../cedictMaps/cedictTradDat.dat"))
;(define readSimp2 (load-data "../cedictMaps/cedictSimpDat.dat"))

;(equal? ha2b readSimp)
;(equal? ha3c readTrad)
;(equal? ha2b readSimp2)
;(equal? ha3c readTrad2)



;(length (hash->list ha2b))
;(length (hash->list ha3c))

"end of cedict calculation"

;;slut
