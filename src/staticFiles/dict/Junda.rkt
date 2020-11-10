#lang racket

(provide jundahash)


(define (jundaHashGen fun_keyindex fun_valindex str_jundafilecontent)
  (let ([li_li_str_numbersandchars
         (jundaContentToNestedList #px"^[0-9]+[\\s]+[\\S]+[\\s]+.*$" str_jundafilecontent)])
    (for/hash ([li_str_eachline li_li_str_numbersandchars])
      (values (fun_keyindex li_str_eachline) (string->number (fun_valindex li_str_eachline))))))


(define (jundaContentToNestedList reg_matchingRegex str_jundaFileContent)
  (map (lambda (str_eachline) (string-split str_eachline #px"[\t]+"))
  (filter (lambda (str_eachline) (regexp-match? reg_matchingRegex str_eachline))  
  (string-split str_jundaFileContent #rx"\n"))))


(define junda (file->string "../../../inputFiles/Junda2005"))
(define jundahash (jundaHashGen second first junda))
(define filterjunda (jundaContentToNestedList  #px"^[0-9]+[\\s]+[\\S]+[\\s]+.*$" junda))
;#px"^[0-9]+[\\s]+\\S[\\s]+.*$"



;(length filterjunda)
;(length (hash-values jundahash))
;(apply + (range 12042))
;(apply + (hash-values jundahash))


;;slut
