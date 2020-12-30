#lang racket

(require racket/fasl)

(define dayi4Raw
  (file->string "../Dayi4Source/dayi4old.txt"))
;(string-length dayi3Raw)

(define li_dayiLines ;(map (lambda (eachLine)
         ;(string-split eachLine #px"[\\s]+"))
 (string-split dayi4Raw #px"\n"))

(define removedBadLines (map (lambda (eachLine) (string-downcase eachLine)) (list-tail li_dayiLines 3)))
(define cleanDayi
  (set->list (list->set
  (map
   (lambda (eachLine) (string-split eachLine)) removedBadLines))))

;(length cleanDayi)
;(length cleanDayi)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;  coden her producere en liste af par: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (producePairs str_toCompare fun_center fun_compare)
  (list str_toCompare

        (set->list (list->set (map
   (lambda (each) (fun_compare each))
  (filter
   (lambda (eachpair)
     (equal? str_toCompare (fun_center eachpair)))
   cleanDayi))))))

(define (centerOnFirstOrSecond fun_itemToCenter fun_itemToCompare)
  (map
   (lambda (eachList) (producePairs (fun_itemToCenter eachList) fun_itemToCenter fun_itemToCompare))
   cleanDayi))

(define chineseFirst (centerOnFirstOrSecond second first))
(define codeFirst (centerOnFirstOrSecond first second))

;chineseFirst
;codeFirst

;chineseFirst

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;   her produceres hashmaps:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (load-data path)
  (call-with-input-file path fasl->s-exp))

(define (createHash li_charToCodesOrCodesToChar)
  (for/hash ([li_eachKeyValuePair li_charToCodesOrCodesToChar])
    (values (first li_eachKeyValuePair)
            (second li_eachKeyValuePair))))

(define testCharHash (createHash chineseFirst))
(define testCodeHash (createHash codeFirst))

(length (hash-keys testCharHash))
(length (hash-keys testCodeHash))

;;;;;;;;;;;; array code locations
;(call-with-output-file "../array30Maps/array30CharHash.rktd"
;  (lambda (out) (s-exp->fasl testCharHash out))
;  #:exists 'replace)
;(call-with-output-file "../array30Maps/array30CodeHash.rktd"
;  (lambda (out) (s-exp->fasl testCodeHash out))
;  #:exists 'replace)
;(define readCharHash (load-data "../array30Maps/array30CharHash.rktd"))
;(define readCodeHash (load-data "../array30Maps/array30CodeHash.rktd"))

;array3 code locations
(call-with-output-file "../Dayi4Maps/dayi4CharHash.rktd"
  (lambda (out) (s-exp->fasl testCharHash out))
  #:exists 'replace)
(call-with-output-file "../Dayi4Maps/dayi4CodeHash.rktd"
  (lambda (out) (s-exp->fasl testCodeHash out))
  #:exists 'replace)

(define readCharHash (load-data "../Dayi4Maps/dayi4CharHash.rktd"))
(define readCodeHash (load-data "../Dayi4Maps/dayi4CodeHash.rktd"))


(equal? (hash-keys testCharHash) (hash-keys readCharHash))
(equal? (hash-keys testCodeHash) (hash-keys readCodeHash))

(hash-ref testCharHash "溫")
(hash-ref testCodeHash "8b.")











;;slut
