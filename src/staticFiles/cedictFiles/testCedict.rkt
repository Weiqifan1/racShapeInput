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

;;;;;;;;;;;;;;;;;;;;;; test af cedict ;;;;;;;;;;;;;

;;; hvis man taeller alle frequency rank elemeneter,
;; faar man faar de traditionelle> 13060
;; og for de simplificerede: 12041
(length relevantFreq)

;;;her er ekstempler paa resultaterne naar man kalder disse eksempler:
(hash-ref stest "A")
(hash-ref stest "长")
(hash-ref stest "黤")
(hash-ref ttest "你好")
(hash-ref ttest "鷍")
(hash-ref ttest "𪜂") ;dette tegn findes ikke i hverken cedict eller tzai. at kalde den vil give en fejl.


#|
'#hash((cedictinfo . (("A" "A" "[A]" "/(slang) (Tw) to steal/")))
       (comparison . (0))
       (corpussize . (0))
       (corpustype . (0))
       (frequencyrank . (0))
       (key . "A")
       (occurrences . (0))
       (unicode . (65)))
'#hash((cedictinfo
        .
        (("長"
          "长"
          "[chang2][zhang3]"
          "/length/long/forever/always/constantly/ [zhang3]/chief/head/elder/to grow/to develop/to increase/to enhance/")))
       (comparison . (111))
       (corpussize . (258852642))
       (corpustype . ("simplified"))
       (frequencyrank . (111))
       (key . "长")
       (occurrences . (435608))
       (unicode . (38271)))
'#hash((cedictinfo . (("黤" "黤" "[]" "/NOTE: the char. is not in cedict. The trad. simp. pair might be wrong")))
       (comparison . (12041))
       (corpussize . (258852642))
       (corpustype . ("simplified"))
       (frequencyrank . (12041))
       (key . "黤")
       (occurrences . (1))
       (unicode . (40676)))
'#hash((cedictinfo . (("你好" "你好" "[ni3 hao3]" "/hello/hi/")))
       (comparison . (19 21))
       (corpussize . (171894734 171894734))
       (corpustype . ("traditional" "traditional"))
       (frequencyrank . (19 21))
       (key . "你好")
       (occurrences . (915385 860232))
       (unicode . (20320 22909)))
'#hash((cedictinfo . (("鷍" "鷍" "[]" "/NOTE: the char. is not in cedict. The trad. simp. pair might be wrong")))
       (comparison . (13060))
       (corpussize . (171894734))
       (corpustype . ("traditional"))
       (frequencyrank . (13060))
       (key . "鷍")
       (occurrences . (4))
       (unicode . (40397)))
; hash-ref: no value found for key
;   key: "𪜂"
; Context (plain; to see better errortrace context, re-run with C-u prefix):
;   /home/pc20201005/Documents/testracket/src/staticFiles/cedictFiles/testCedict.rkt:1:0 [running body]

|#



;slut
