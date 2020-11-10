#lang racket

(provide tzaihash)


(define (tzaiHashGen int_start int_end str_cedictFileContent)
  (let ([li_str_tzaiCharNum (tzaiNumGen int_start int_end str_cedictFileContent)])
    (for/hash ([str_each li_str_tzaiCharNum])
      (values (first (string-split str_each " ")) (string->number (second (string-split str_each " ")) )))))

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
(define tzaihash (tzaiHashGen 0 1 tzai))

;(length tzailist)
;(first tzainum)
;(last tzainum)
;(hash-ref tzaihash "的")
;(hash-ref tzaihash "鷍")

;(apply + (range 13061))
;(apply + (hash-values tzaihash))


;鷍

;;slut
