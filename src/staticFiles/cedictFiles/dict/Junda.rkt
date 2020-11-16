#lang racket

(provide jundahash)

;(define (jundaHashGen fun_keyindex fun_valindex str_jundafilecontent)
;  (let ([li_li_str_numbersandchars
;         (jundaContentToNestedList #px"^[0-9]+[\\s]+[\\S]+[\\s]+.*$" str_jundafilecontent)])
;    (for/hash ([li_str_eachline li_li_str_numbersandchars])
;      (values (fun_keyindex li_str_eachline) (string->number (fun_valindex li_str_eachline))))))

(define (jundaHashGen fun_keyindex
                      str_corpustype
                      str_corpussize
                      str_jundafilecontent)
  (let ([li_li_str_numbersandchars
         (jundaContentToNestedList #px"^[0-9]+[\\s]+[\\S]+[\\s]+.*$" str_jundafilecontent)])
    (for/hash ([li_str_eachline li_li_str_numbersandchars])
      (values (fun_keyindex li_str_eachline)
              (hash
               'corpustype str_corpustype
               'corpussize str_corpussize
               'frequencyrank (string->number (first li_str_eachline))
               'character (second li_str_eachline)
               'occurrences (string->number (third li_str_eachline))
               )))))


(define (jundaContentToNestedList reg_matchingRegex str_jundaFileContent)
  (map (lambda (str_eachline) (string-split str_eachline #px"[\t]+"))
  (filter (lambda (str_eachline) (regexp-match? reg_matchingRegex str_eachline))  
  (string-split str_jundaFileContent #rx"\n"))))


(define officialTotalNumber 258852642)
(define junda (file->string "../cedictSource/Junda2005"))
;(define junda (file->string "../cedictSource/testjunda"))
(define filterjunda (jundaContentToNestedList  #px"^[0-9]+[\\s]+[\\S]+[\\s]+.*$" junda))
;#px"^[0-9]+[\\s]+\\S[\\s]+.*$"



;(length (hash-values jundahash))
;(apply + (range 12042))
;(apply + (hash-values jundahash))

;(first filterjunda)

;(define deOccurrences (string->number (third (first filterjunda))))
;(identity deOccurrences)

(define calculatedTotalNumber
  (apply +
  (map
   (lambda (li_eachList)
     (string->number (third li_eachList)))
   filterjunda)))


(define jundahash (jundaHashGen second "simplified" officialTotalNumber junda))


;(length filterjunda)
;(hash-ref jundahash  "的")
;(identity officialTotalNumber)
;(identity calculatedTotalNumber)
;(equal? officialTotalNumber calculatedTotalNumber)

;;slut
