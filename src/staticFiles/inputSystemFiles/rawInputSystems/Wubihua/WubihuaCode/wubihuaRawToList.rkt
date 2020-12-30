#lang racket

(require racket/fasl)


(define wubihua
  (file->string "../WubihuaSource/wubihuaOld.txt"))
;(string-length dayi3Raw)

;(define li_dayiLines ;(map (lambda (eachLine)
;         ;(string-split eachLine #px"[\\s]+"))
; (string-split dayi3Raw #px"\n"))
;(length li_dayiLines) ;18156

(define wubihualines (string-split wubihua #px"\n"))
;wubihualines

(define splitwubihua
  (map
   (lambda (eachLine)
     (string-split eachLine))
   wubihualines))

(define removebig (filter (lambda (eachLine) (> 4 (length eachLine)))splitwubihua))
(define removetiny (filter (lambda (each) (< 1 (length each))) removebig))
(define removebad (filter (lambda (each) (or (equal? 3 (length each))
                                             (equal? 1 (string-length (second each))))) removetiny))

(length removebig)
(length removetiny)
(length removebad)

;;skriv en funktion der fjerne elementer hvor index 0 hartegn udover nm,./
(define (hasonly5 str_input)
  (and (not (equal? "" str_input))  (equal? '()
  (filter
   (lambda (eachchar) (and (not (equal? #\m eachchar))
                        (not (equal? #\n eachchar))
                        (not (equal? #\, eachchar))
                        (not (equal? #\. eachchar))
                        (not (equal? #\/ eachchar))))
   (string->list str_input)))))

(define onlywantedlines (filter (lambda (each)
                               (hasonly5 (first each))) removebad))

(define cleanwubihua
  (map
   (lambda (each)(take each 2))
   onlywantedlines))

;(length onlywantedlines)
;(length cleanwubihua)

;cleanwubihua

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
   cleanwubihua))))))

(define (centerOnFirstOrSecond fun_itemToCenter fun_itemToCompare)
  (map
   (lambda (eachList) (producePairs (fun_itemToCenter eachList) fun_itemToCenter fun_itemToCompare))
   cleanwubihua))

(define chineseFirst (centerOnFirstOrSecond second first))
(define codeFirst (centerOnFirstOrSecond first second))

;(length chineseFirst)
;(length codeFirst)

;chineseFirst
;codeFirst
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
(call-with-output-file "../WubihuaMaps/wubihuaCharHash.rktd"
  (lambda (out) (s-exp->fasl testCharHash out))
  #:exists 'replace)
(call-with-output-file "../WubihuaMaps/wubihuaCodeHash.rktd"
  (lambda (out) (s-exp->fasl testCodeHash out))
  #:exists 'replace)

(define readCharHash (load-data "../WubihuaMaps/wubihuaCharHash.rktd"))
(define readCodeHash (load-data "../WubihuaMaps/wubihuaCodeHash.rktd"))


(equal? (hash-keys testCharHash) (hash-keys readCharHash))
(equal? (hash-keys testCodeHash) (hash-keys readCodeHash))

(hash-ref testCharHash "溫")
(hash-ref testCodeHash "/nm.,")




;;slut
