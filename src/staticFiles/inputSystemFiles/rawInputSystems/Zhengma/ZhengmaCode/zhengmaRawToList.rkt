#lang racket


(require racket/fasl)

(define zhengmaraw
  (file->string "../ZhengmaSource/zz201906_allcodesOld.txt"))
;(string-length dayi3Raw)


(define lines ;(map (lambda (eachLine)
         ;(string-split eachLine #px"[\\s]+"))
 (string-split zhengmaraw #px"\n"))
;(length li_dayiLines) ;18156

;(length lines)
;(take lines 100)
;(drop lines 49800)

;create a function that checks if a string stats with the chars "\
(define (stringstartwith str_inputstring str_start)
  (let* ([inputlist (string->list str_inputstring)]
         [strstart (string->list str_start)]
         [len (length strstart)])
    (if (< (length inputlist) (length strstart))
        #f
        (equal? len (length
                     (filter (lambda
                                 (eachindex)
                               (equal? (list-ref inputlist eachindex)
                                       (list-ref strstart eachindex))) (range len)))))))
;(stringstartwith "abcdefg" "abdc")

;create a function that filter away unwanted characters
(define (removechars str_input str_badchars)
  (let* ([inputlist (string->list str_input)]
         [badchars (string->list str_badchars)])
    (list->string (remove* badchars inputlist))))
;(removechars "abcdefg" "ce")


(define rightlines (filter
                    (lambda (eachlines)
                      (stringstartwith eachlines "\"")) lines))
(length lines)
(length rightlines)

(define devidegoodlines
  (filter (lambda (each) (< 2 (string-length each)))
          (flatten (map (lambda (eachline) (string-split eachline ",<")) rightlines))));(string-split rightlines ",<")))

(length devidegoodlines)

(define cleanzhengma
  (map (lambda (each)
         (string-split
          (removechars each "\"<>")
          "=")) devidegoodlines))
;(take cleanzhengma 300)



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
   cleanzhengma))))))

(define (centerOnFirstOrSecond fun_itemToCenter fun_itemToCompare)
  (map
   (lambda (eachList) (producePairs (fun_itemToCenter eachList) fun_itemToCenter fun_itemToCompare))
   cleanzhengma))

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
;(call-with-output-file "../ZhengmaMaps/zhengmaCharHash.rktd"
;  (lambda (out) (s-exp->fasl testCharHash out))
;  #:exists 'replace)
;(call-with-output-file "../ZhengmaMaps/zhengmaCodeHash.rktd"
;  (lambda (out) (s-exp->fasl testCodeHash out))
;  #:exists 'replace)
;(define readCharHash (load-data "../ZhengmaMaps/zhengmaCharHash.rktd"))
;(define readCodeHash (load-data "../ZhengmaMaps/zhengmaCodeHash.rktd"))

;array3 code locations
(call-with-output-file "../ZhengmaMaps/zhengmaCharHash.rktd"
  (lambda (out) (s-exp->fasl testCharHash out))
  #:exists 'replace)
(call-with-output-file "../ZhengmaMaps/zhengmaCodeHash.rktd"
  (lambda (out) (s-exp->fasl testCodeHash out))
  #:exists 'replace)

(define readCharHash (load-data "../ZhengmaMaps/zhengmaCharHash.rktd"))
(define readCodeHash (load-data "../ZhengmaMaps/zhengmaCodeHash.rktd"))


(equal? (hash-keys testCharHash) (hash-keys readCharHash))
(equal? (hash-keys testCodeHash) (hash-keys readCodeHash))

(hash-ref testCharHash "溫")
(hash-ref testCodeHash "af")









;slut
