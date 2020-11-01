#lang racket
(require racket/file)
;(require compose-app)

;cnagjie fil CangJie_textCHR.txt

(define (extract str)
  (substring str 4 7))

(define (chrtest myint) (+ 2 myint))

(define myvar (chrtest 30))


(define (next-line-it file)
  (let ((line (read-line file 'any)))
    (unless (eof-object? line)
      (displayln line)
      (next-line-it file))))

;(define firsttry (call-with-input-file "testdoc" next-line-it))

;;;;;;;;;;;;;;;;;;; 2020-10-31 ;;;;;;;;;;;;;;;;;

;opgaver:
;* laes alle linjerne fra filen (done)
;* fjerne byteordermark hvis det er der (start med at finde laengden paa den foerste string)
;* lav en regex der matcher @engelsk kinesisk@ med praecis 1 mellemrum
;* find ud af hvordan man kun laeser en del af en liste (som @take@ i clojure og haskell)
;* se om jeg kan bruge regex til at sortere i den store cangjie fil

;(define (hashFromFile
;         str_filename
;         reg_chineseFirst
;         reg_chineseLast)
;  (map listWithCodeFirstChineseLast
;  (filter
;        (lambda (str_eachClean) (isAcceptableChineseLine str_eachClean reg_chineseFirst reg_chineseLast))
;  (cleanStringBeforeHashmaping
;  (file->lines str_filename)))))


;;;;;;;;;;;;;;;;

(define cangjie2 (file->lines "testdoc"))
(define (removeByteOrderMark listFromFile)
  (if (regexp-match? #rx"\uFEFF" (car listFromFile))
                     (cons
                  (substring (car listFromFile) 1)
                  (list-tail listFromFile 1))
                     listFromFile))
(define cangjie3 (removeByteOrderMark cangjie2))


(define (removeUndesiredChars str_input str_badChars)
  (list->string (remove* (string->list str_badChars) (string->list str_input))))

(define (removeCharsFromList li_str_input str_badChars)
  (map (lambda (str_each) (removeUndesiredChars str_each str_badChars)) li_str_input))

(define (cleanStringBeforeHashmaping li_str_input)
  (map string-downcase
   (removeCharsFromList li_str_input "\uFEFF#"))
  )

(define cangjie4
  (cleanStringBeforeHashmaping cangjie2))

;(regexp-match? #rx"#+" "#abc")
;(regexp-match? #rx"#+" "a 土bc")

;"g 土"
;lav en regex der matcher en engelsk string + mellemrum + kinesisk
;(regexp-match? #rx"([a-z0-9\\.,;])" "abc")

(define (isChineseFirst str_input regegx) (regexp-match? regex str_input))
(define (isChineseLast str_input regex) (regexp-match? regex str_input))

(define (isAcceptableChineseLine str_input regexChineseFirst regexChineseLast)
  (cond
    [(isChineseLast str_input regexChineseFirst regexChineseLast) #t]
    [(isChineseFirst str_input regexChineseFirst regexChineseLast) #t]
    [else #f]))

(define (listWithCodeFirstChineseLast str_input )
  (cond
    [(isChineseLast str_input) (string-split str_input #px"[\\s]+")]
    [(isChineseFirst str_input) (reverse (string-split str_input #px"[\\s]+"))]
    [else (error (string-join (list "error: this line fails in listWithCodeFirst... " str_input)))]
    ))


;(define cangjie5 (map (lambda (str_each) (listWithCodeFirstChineseLast str_each)) cangjie4))

   ;    (isCisChineseFirst str_input)
    ;   (isChineseLast str_input)
     ;  ) #t #f))

(define cangjie5
  (filter isAcceptableChineseLine cangjie4 #px"^[\\S]+[\\s]+[a-z0-9\\.,/;]+$" #px"^[a-z0-9\\.,/;]+[\\s]+[\\S]+$"))

(define abandonedLines
  (filter (lambda (str_each) (not (isAcceptableChineseLine str_each #px"^[\\S]+[\\s]+[a-z0-9\\.,/;]+$" #px"^[a-z0-9\\.,/;]+[\\s]+[\\S]+$"))) cangjie4))

(define cangjie6
 (map listWithCodeFirstChineseLast cangjie5 ))

(define cangjie7
  (for/hash ([li_str_each cangjie6])
    (values (first li_str_each) (second li_str_each))))

(hash-ref cangjie7 "aniq")
  

;((lambda (inp) (if (isChineseLast inp) (string-split inp #px"[\\s]+") (list "hi" "there")))  "u 山")

;;opg 1  

















;;slut
