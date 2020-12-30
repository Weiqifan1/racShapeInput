#lang racket


(require racket/fasl)

(define wubiraw
  (file->string "../WubiSource/WubiCodesOld.txt"))
;(string-length dayi3Raw)

(define wubilines ;(map (lambda (eachLine)
         ;(string-split eachLine #px"[\\s]+"))
 (string-split wubiraw #px"\n"))
;(length li_dayiLines) ;18156

;(length wubilines)


(define (hasonlysmallalpha str_input)
  (and (not (equal? "" str_input))  (equal? (string-length str_input)(length
  (filter
   (lambda (eachchar) (and (< 96 (char->integer eachchar))
                           (> 123 (char->integer eachchar)) ))
   (string->list str_input))))))

;(hasonlysmallalpha "")
;(hasonlysmallalpha "ert")
;(hasonlysmallalpha "er4t")
;(hasonlysmallalpha "sdAf")


(define nestedwubi
  (map
   (lambda (eachString) (string-split eachString))
   wubilines))

;(filter
; (lambda (each) (not (equal? 3 (length each))))
; nestedwubi) 

(define removewrongfirsts
  (filter (lambda (eachlist)
            (and (< 1 (length eachlist))
                 (hasonlysmallalpha (first eachlist)))) nestedwubi))

(define test1
  (filter (lambda (eachlist)
            (or (> 2 (length eachlist))
                 (not (hasonlysmallalpha (first eachlist))))) nestedwubi))


(length nestedwubi)
(length removewrongfirsts)
(length test1)
;test1
;(take removewrongfirsts 100)
;(list-tail removewrongfirsts 80100)
(define cleanwubi
  (map
   (lambda (eachlist)
     (take eachlist 2))
   removewrongfirsts))

;(length cleanwubi)
;(take cleanwubi 100)

;(length (filter (lambda (each) (not (equal? 2 (length cleanwubi)))) cleanwubi))


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
   cleanwubi))))))

(define (centerOnFirstOrSecond fun_itemToCenter fun_itemToCompare)
  (map
   (lambda (eachList) (producePairs (fun_itemToCenter eachList) fun_itemToCenter fun_itemToCompare))
   cleanwubi))

(define chineseFirst (centerOnFirstOrSecond second first))
(define codeFirst (centerOnFirstOrSecond first second))

;(length chineseFirst)
;(length codeFirst)

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
(call-with-output-file "../WubiMaps/wubiCharHash.rktd"
  (lambda (out) (s-exp->fasl testCharHash out))
  #:exists 'replace)
(call-with-output-file "../WubiMaps/wubiCodeHash.rktd"
  (lambda (out) (s-exp->fasl testCodeHash out))
  #:exists 'replace)

(define readCharHash (load-data "../WubiMaps/wubiCharHash.rktd"))
(define readCodeHash (load-data "../WubiMaps/wubiCodeHash.rktd"))


(equal? (hash-keys testCharHash) (hash-keys readCharHash))
(equal? (hash-keys testCodeHash) (hash-keys readCodeHash))

(hash-ref testCharHash "溫")
(hash-ref testCodeHash "ax")




;slut
