#lang racket
(require csv-reading)
(require racket/fasl)



(define (listFromCSVFilepath csvFilePathStr)
  (csv->list
  (make-csv-reader
   (open-input-file csvFilePathStr)
   '((separator-chars            #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t)))))


(define hlist (listFromCSVFilepath "../heisigSource/heisigInfo.csv"))
;(length hlist)

(define (removeTraditionalCharsWithNoNumbers nestedList)
  (filter
   (lambda (eachList) (not (or
                       (equal? "" (list-ref eachList 0))
                       (equal? "" (list-ref eachList 3)))))
   nestedList))
(define THonly (removeTraditionalCharsWithNoNumbers hlist))
;(length THonly)

(define (correctTraditionalNumbers nestedLines) 
  (filter
   (lambda (eachCorrectTH)
     (number? (string->number (list-ref eachCorrectTH 0))))
   nestedLines))
(define notNumbers (correctTraditionalNumbers THonly))
;(length notNumbers)

(define shrink
  (map
   (lambda (eachLines)
     (list (list-ref eachLines 0)
           (list-ref eachLines 3)
           (list-ref eachLines 7)
           (list-ref eachLines 2)
           (list-ref eachLines 5)
           (list-ref eachLines 9)))
   notNumbers))

(length shrink)
(last shrink)

(define tradHashMap
  (for/hash ([each shrink])
    (values
     (list-ref each 1)
     (hash 'heisigchar (list-ref each 1)
           'heisignumber (string->number (list-ref each 0))
           'heisigmeaning (list-ref each 2)))))
;(hash-ref tradHashMap "宿")

;;;;;;;;;;;;;;;;;; i now have a hashmap of traditional characters. I now need a list of
;;simplified characters


(define (removeSimplifiedCharsWithNoNumbers nestedList)
  (filter
   (lambda (eachList) (not (or
                       (equal? "" (list-ref eachList 1))
                       (equal? "" (list-ref eachList 4)))))
   nestedList))
(define SHonly (removeSimplifiedCharsWithNoNumbers hlist))
;(length SHonly)


(define (correctSimplifiedlNumbers nestedLines) 
  (filter
   (lambda (eachCorrectSH)
     (number? (string->number (list-ref eachCorrectSH 1))))
   nestedLines))
(define notNumbersSimp (correctSimplifiedlNumbers SHonly))
;(length notNumbersSimp)
;(list-ref notNumbersSimp 3000)

(define simpHashMap
  (for/hash ([each notNumbersSimp])
    (values (list-ref each 4)
            (hash 'heisigchar (list-ref each 4)
                  'heisignumber (string->number (list-ref each 1))
                  'heisigmeaning (list-ref each 7)))))

;(hash-ref simpHashMap "畴")
;(length (hash-keys simpHashMap))

;;;;;;;;; nu skal jeg teste om de to hashmaps er korrekte.
;jeg vil lave en liste over alle tallene og se om summen er
;1-3035 og 1-3018 henholdsvis

(define (sumOfAllnumbers tradOrSimpMap)
  (apply +
  (map
   (lambda (eachHashmap)
     (hash-ref eachHashmap 'heisignumber))
   (hash-values tradOrSimpMap))))

;;;test om tallene er korrekte. de 2 equal operationer skal returnere true
(define tradSum (sumOfAllnumbers tradHashMap))
(define simpSum (sumOfAllnumbers simpHashMap))
(equal? tradSum (apply + (range 0 3036)))
(equal? simpSum (apply + (range 0 3019)))

;;;;;;;;;;;;;;;;; nu har jeg den 'oenskede hashmaps. nu skal de gemmes

;gemt de 2 hashmaps somp hurtigt serialiseret, og se om de er ens
;(call-with-output-file "../heisigMaps/heisigTradSerial.rktd"
;  (lambda (out) (s-exp->fasl tradHashMap out)) #:exists 'replace)

;(call-with-output-file "../heisigMaps/heisigSimpSerial.rktd"
;  (lambda (out) (s-exp->fasl simpHashMap out)) #:exists 'replace)

(define (load-data path)
  (call-with-input-file path fasl->s-exp))

(define readTrad (load-data "../heisigMaps/heisigTradSerial.rktd"))
(define readSimp (load-data "../heisigMaps/heisigSimpSerial.rktd"))

(equal? tradHashMap readTrad)
(equal? simpHashMap readSimp)

;nu skal jeg eksprotere heisig til andre moduler

;;slut
