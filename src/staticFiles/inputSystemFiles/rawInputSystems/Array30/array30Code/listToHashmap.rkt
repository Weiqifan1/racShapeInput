#lang racket

(require racket/fasl)

(define (load-data path)
  (call-with-input-file path fasl->s-exp))

(define readCharList (load-data "../array30Maps/array30CharList.rktd"))
(define readCodeList (load-data "../array30Maps/array30CodeList.rktd"))

(first readCharList)
;(length readCharList)
(first readCodeList)
;(length readCodeList)

(define (cleanList fun_codeOrChar li_li_charToCodeOrCodeToChar)
  (map
   (lambda (eachList)
     (list (first eachList)
           (map
            (lambda (eachSubList)
              (fun_codeOrChar eachSubList))
            (second eachList))))
   li_li_charToCodeOrCodeToChar))

(define testCleanChar
  (cleanList first readCharList))

(define testCleanCode
  (cleanList second readCodeList))

(first testCleanChar)
(first testCleanCode)
(length (set->list (list->set testCleanChar)))
(length (set->list (list->set testCleanCode)))

;;;;;;;;;; create hashmaps from clean lists

(define (createHash li_charToCodesOrCodesToChar)
  (for/hash ([li_eachKeyValuePair li_charToCodesOrCodesToChar])
    (values (first li_eachKeyValuePair)
            (second li_eachKeyValuePair))))

(define testCharHash (createHash testCleanChar))
(define testCodeHash (createHash testCleanCode))

(length (hash-keys testCharHash))
(length (hash-keys testCodeHash))

;(call-with-output-file "../array30Maps/array30CharHash.rktd"
;  (lambda (out) (s-exp->fasl testCharHash out))
;  #:exists 'replace)
;(call-with-output-file "../array30Maps/array30CodeHash.rktd"
;  (lambda (out) (s-exp->fasl testCodeHash out))
;  #:exists 'replace)

(define readCharHash (load-data "../array30Maps/array30CharHash.rktd"))
(define readCodeHash (load-data "../array30Maps/array30CodeHash.rktd"))

(equal? (hash-keys testCharHash) (hash-keys readCharHash))
(equal? (hash-keys testCodeHash) (hash-keys readCodeHash))

(hash-ref testCharHash "溫")
(hash-ref testCodeHash "cpu")

;slut
