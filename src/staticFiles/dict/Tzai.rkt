#lang racket

(provide tzaihash)


#|

(define (tzaiNumGen int_start int_end str_cedictFileContent)
  (let* ([tzailist (tzaiTrimmed int_start int_end str_cedictFileContent)]
         [tzaivec (list->vector tzailist)])
    (map (lambda (str_each) (string-append str_each " " (format "~v" (+ 1 (vector-member str_each tzaivec)))))
         tzailist)))

(define (tzaiTrimmed int_start int_end str_cedictFileContent)
  ;(hashFromCedict fun_chooseKey
  ;(filter (lambda (li_str_each) (< 3 (length li_str_each)))
  ;(map (lambda (str_eachLine) (regexp-split #rx"###" str_eachLine))
  ;(let ([count 1]) ()
  (map (lambda (str_each) (substring str_each int_start int_end))
  (regexp-split #rx"\n" (substring str_cedictFileContent 1))))

(define tzai (file->string "../../../inputFiles/Tzai2006.txt"))

(define tzailist (tzaiTrimmed 0 1 tzai))
(define tzainum (tzaiNumGen 0 1 tzai))
;(define tzaihash (tzaiHashGen 0 1 tzai))

(length tzailist)
;(first tzainum)
;(last tzainum)
;(hash-ref tzaihash "的")
;(hash-ref tzaihash "鷍")

;(apply + (range 13061))
;(apply + (hash-values tzaihash))
;鷍



(define (tzaiSplitLines str_tzaiText)
  (map (lambda (str_eachLine)
         (string-split str_eachLine))
  (regexp-split #rx"\n" (substring str_tzaiText 1))))

(define (corpusSize str_tzaiText)
 (apply + (map (lambda (li_str_eachLine) (string->number (second li_str_eachLine)))(tzaiSplitLines str_tzaiText))))

(corpusSize tzai)
(second (first (tzaiSplitLines tzai)))


(define (tzaiNumGen2 str_cedictFileContent)
  (let* ([tzailist (tzaiSplitLines str_cedictFileContent)]
         [tzaivec (list->vector tzailist)]
         [int_corpusSize (corpusSize str_cedictFileContent)])
    (map
     (lambda (li_each) (cons (number->string int_corpusSize) (cons (format "~v" (+ 1 (vector-member li_each tzaivec))) li_each )))
         tzailist)))

(first (tzaiNumGen2 tzai))


(define (tzaiHashGen str_cedictFileContent)
  (let ([li_li_str_tzaiCharNum (tzaiNumGen2 str_cedictFileContent)])
    (for/hash ([li_str_each li_li_str_tzaiCharNum])
      (values (third (li_str_each))
              (hash 'frequency (second li_str_each)
                    'occurrences (fourth li_str_each)
                    'corpussize (first li_str_each)) ))))

;(define (tzaiHashGen2 str_cedictFileContent)
;  (let ([li_li_str_tzaiCharNum (tzaiNumGen2 str_cedictFileContent)])
;    (for/hash ([li_str_each li_li_str_tzaiCharNum])
;      (values (third (li_str_each))
;              (list (second li_str_each)
;                     (fourth li_str_each)
 ;                    (first li_str_each)) ))))
;

;(define tzaihash (tzaiHashGen2 tzai))

;(length (hash-keys (tzaiHashGen tzai)))

|#
;;;;;;;;;;;;;;;slriv en ny version af tzai hash der medtager  ;;;;;;;;;;;;;;;;;;
(define tzai (file->string "../../../inputFiles/Tzai2006.txt"))

(define (tzaiSplitLines str_tzaiText)
  (map (lambda (str_eachLine)
         (string-split str_eachLine))
  (regexp-split #rx"\n" (substring str_tzaiText 1))))

(define (corpusSize str_tzaiText)
 (apply + (map (lambda (li_str_eachLine) (string->number (second li_str_eachLine)))(tzaiSplitLines str_tzaiText))))

(corpusSize tzai)
(second (first (tzaiSplitLines tzai)))
(first (tzaiSplitLines tzai))

(define (tzaiNumGen2 str_cedictFileContent)
  (let* ([tzailist (tzaiSplitLines str_cedictFileContent)]
         [tzaivec (list->vector tzailist)]
         [int_corpusSize (corpusSize str_cedictFileContent)])
    (map
     (lambda (li_each)
       (cons (number->string int_corpusSize)
             (cons (format "~v" (+ 1 (vector-member li_each tzaivec)))
                   li_each)))
         tzailist)))

(define mergedList (tzaiNumGen2 tzai))
(length mergedList)
(first mergedList)

(define (tzaiHashGen str_cedictFileContent)
  (let ([li_li_eachline (tzaiNumGen2 str_cedictFileContent)])
    (for/hash ([each li_li_eachline])
      (values (third each)
              (list (first each)
                    (second each)
                    (third each)
                    (fourth each))))))

(define (tzaiHashGen2 str_corpustype str_cedictFileContent)
  (let ([li_li_eachline (tzaiNumGen2 str_cedictFileContent)])
    (for/hash ([each li_li_eachline])
      (values (third each)
              (hash 'corpustype str_corpustype
                    'corpussize (string->number (first each))
                    'frequency (string->number (second each))
                    'character (third each)
                    'occurrences(string->number (fourth each)))))))


(define tzaihash (tzaiHashGen2 "traditional" tzai))
 (hash-ref tzaihash "的")

;;slut
