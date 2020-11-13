#lang racket

(require racket/fasl) ;used for serializing data - look at the bottom of the file


(+ 2 2)


;;;;;;; write code to serialize data properly ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;https://www.travishinkelman.com/data-serialization-in-r-and-racket/
; define function for saving data to a rkdt file                     

;(save-rktd nested-hash "hash.rktd")            ; write hash table to file
;(equal? nested-hash (read-rktd "hash.rktd"))   ; read hash table from file and compare t

(define (load-data path)
  (call-with-input-file path fasl->s-exp))

(define ttest (load-data "cedictMaps/cedictTradSerial.rktd"))
(define stest (load-data "cedictMaps/cedictSimpSerial.rktd"))

;(define tdat (load-data "cedictMaps/cedictTradDat.dat"))
;(define sdat (load-data "cedictMaps/cedictSimpDat.dat"))

;(define tt (hash-keys ttest))
;(define st (hash-keys stest))
;(length (hash-keys ttest))
;(length (hash-keys stest))
;"我"
;(hash-ref stest "发")
;(hash-ref ttest "髮")
;(hash-ref ttest "發")

(+ 3 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO>
;1 check if a  junda and tzai characters are in cedict
;2 add the junda and tzai proportions for each char to the cidtionaries


;;;;;;;;;;;;;;;;;;;;;;;;;;;


;slut
