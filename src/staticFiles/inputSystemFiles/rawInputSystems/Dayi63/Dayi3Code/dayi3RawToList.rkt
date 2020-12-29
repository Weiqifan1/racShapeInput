#lang racket

(require racket/fasl)

(define dayi3Raw
  (file->string "../Dayi3Source/dayi6_3codeOld.txt"))
;(string-length dayi3Raw)

(define li_dayiLines ;(map (lambda (eachLine)
         ;(string-split eachLine #px"[\\s]+"))
 (string-split dayi3Raw #px"\n"))
;(length li_dayiLines) ;18156

(define (hasChar? str_input char_input) (if (number? (index-of (string->list str_input) char_input))
                                 #t #f))

(define tabLines
  (filter
   (lambda (eachString)
     (and (hasChar? eachString #\tab) (not (hasChar? eachString #\space))))
   li_dayiLines))

(define spaceLines
  (filter
   (lambda (eachString)
     (and (hasChar? eachString #\space) (not (hasChar? eachString #\tab))))
   li_dayiLines))

(define nonSpaceNonTab
  (filter
   (lambda (eachLines)
     (not (or
           (hasChar? eachLines #\tab)
           (hasChar? eachLines #\space))))
   li_dayiLines))

;(+ (length tabLines) (length spaceLines))
;(length nonSpaceNonTab)

(define li_li_strings
  (map
   (lambda (eachString) (string-split eachString))
   li_dayiLines))
;test if there are any sub lists of a length different than 2 
(filter
 (lambda (eachList) (not (equal? 2 (length eachList))))
 li_li_strings)

;create a function that tests if a string has a character that is not above extended ascii (256)
(define (hasCharAbove256? str_input)
  (< 0 (length (filter
   (lambda (char_each) (< 256 (char->integer char_each)))
   (string->list str_input)))))

(define (acceptableInputSystemElem str_input)
  (or (hasCharAbove256? str_input)
      (equal? 1 (string-length str_input))))

;(hasCharAbove256? "asdĆjk@@l3254")

;clean version of dayi3
(define cleanDayi
  (set->list
  (list->set (filter
   (lambda (eachList) (acceptableInputSystemElem (second eachList)))
   li_li_strings))))

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
;(call-with-output-file "../Dayi3Maps/dayi3CharHash.rktd"
;  (lambda (out) (s-exp->fasl testCharHash out))
;  #:exists 'replace)
;(call-with-output-file "../Dayi3Maps/dayi3CodeHash.rktd"
;  (lambda (out) (s-exp->fasl testCodeHash out))
;  #:exists 'replace)

(define readCharHash (load-data "../Dayi3Maps/dayi3CharHash.rktd"))
(define readCodeHash (load-data "../Dayi3Maps/dayi3CodeHash.rktd"))


(equal? (hash-keys testCharHash) (hash-keys readCharHash))
(equal? (hash-keys testCodeHash) (hash-keys readCodeHash))

(hash-ref testCharHash "溫")
(hash-ref testCodeHash "f9x")



;slut
