#lang racket

(provide tzaihash)

(define tzai (file->string "../cedictSource/Tzai2006.txt"))
;(define tzai (file->string "../cedictSource/testtzai"))

(define (tzaiSplitLines str_tzaiText)
  (filter (lambda (li_each) (< 2 (length li_each)))
  (map (lambda (str_eachLine)
         (string-split str_eachLine))
  (regexp-split #rx"\n" (substring str_tzaiText 1)))))

;(tzaiSplitLines tzai)

(define (corpusSize str_tzaiText)
 (apply + (map (lambda (li_str_eachLine) (string->number (second li_str_eachLine)))(tzaiSplitLines str_tzaiText))))

;(corpusSize tzai)
;(second (first (tzaiSplitLines tzai)))
;(first (tzaiSplitLines tzai))

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

;(define mergedList (tzaiNumGen2 tzai))
;(length mergedList)
;(first mergedList)

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
                    'frequencyrank (string->number (second each))
                    'character (third each)
                    'occurrences(string->number (fourth each)))))))


(define tzaihash (tzaiHashGen2 "traditional" tzai))

;(hash-ref tzaihash "的")

;;slut
