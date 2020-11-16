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

(define ttest (load-data "cedictFiles/cedictMaps/cedictTradSerial.rktd"))
(define stest (load-data "cedictFiles/cedictMaps/cedictSimpSerial.rktd"))

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

;(hash-ref ttest "A")

;testCharacters:
(define (getFrequencyRank ha_hash str_text)
  (hash-ref 
   (hash-ref ha_hash str_text) 'frequencyrank))

(define (allSystemItems ha_hash sym_systemsymbol)
  (flatten
  (filter
   (lambda (li_freqnumbers)
     (and (equal? 1 (length li_freqnumbers))
          (< 0 (first li_freqnumbers))))
  (hash-map
   (hash-values ha_hash)
   (lambda (ha_eachItem)
     (hash-ref ha_eachItem sym_systemsymbol))))))

(define allValues (hash-values stest))
(define allFreqrank
  (map
   (lambda (ha_eachmap)
     (hash-ref ha_eachmap 'frequencyrank))
   allValues))
(define relevantFreq
  (set->list
  (list->set
  (filter
   (lambda (li_number)
     (and (equal? 1 (length li_number))
          (< 0 (first li_number))))
   allFreqrank))))
(length relevantFreq);13060 ;12041

;(define tfreqnum (allSystemItems ttest 'frequencyrank))
;(length tfreqnum)

;(define Aitem (hash-ref ttest "A"))

(hash-ref ttest "A")
(hash-ref ttest "我")



"end of test"
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO>
;1 check if a  junda and tzai characters are in cedict
;2 add the junda and tzai proportions for each char to the cidtionaries


;;;;;;;;;;;;;;;;;;;;;;;;;;;


;slut
