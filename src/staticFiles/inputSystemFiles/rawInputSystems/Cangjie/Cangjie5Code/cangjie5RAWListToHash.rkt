#lang racket

(require racket/fasl)


(define (load-data path)
  (call-with-input-file path fasl->s-exp))

(define readCharList (set->list (list->set (load-data "../Cangjie5Maps/cangjie5CharList.rktd"))))
(define readCodeList (set->list (list->set (load-data "../Cangjie5Maps/cangjie5CodeList.rktd"))))

;(first readCharList)
;(length readCharList)
;(first readCodeList)
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

;(first testCleanChar)
;(first testCleanCode)


(define (createHash li_charToCodesOrCodesToChar)
  (for/hash ([li_eachKeyValuePair li_charToCodesOrCodesToChar])
    (values (first li_eachKeyValuePair)
            (second li_eachKeyValuePair))))

(define testCharHash (createHash testCleanChar))
(define testCodeHash (createHash testCleanCode))

;(length (hash-keys testCharHash))
;(length (hash-keys testCodeHash))
;(hash-ref testCharHash "我")
;(hash-ref testCodeHash "hqi")

;(call-with-output-file "../Cangjie5Maps/cangjie5CharHash.rktd"
;  (lambda (out) (s-exp->fasl testCharHash out))
;  #:exists 'replace)
;(call-with-output-file "../Cangjie5Maps/cangjie5CodeHash.rktd"
;  (lambda (out) (s-exp->fasl testCodeHash out))
;  #:exists 'replace)


(define readCharHash (load-data "../Cangjie5Maps/cangjie5CharHash.rktd"))
(define readCodeHash (load-data "../Cangjie5Maps/cangjie5CodeHash.rktd"))

(equal? readCharHash testCharHash)
(equal? readCodeHash testCodeHash)

(hash-ref testCharHash "我")
(hash-ref testCodeHash "hqi")

(hash-ref readCharHash "我")
(hash-ref readCodeHash "hqi")


;(first readCharList)

;slut
