#lang racket

(provide getListOfCodesFromCode)

(define usedChars (list #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
                        #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0
                        #\; #\, #\. #\/
                        #\- #\[ #\] #\' #\\ #\` #\=))

(define usedCharsTest (list #\a #\b #\c))

(define testList (string->list "ab*cd*"))


;(getListOfCodesFromCode "abc*d")
;(repStar usedCharsTest (list "ab*c**"))




(define (replaceChar li_input int_pos char_new)
  (list->string
  (flatten
  (list (take li_input int_pos) char_new  (drop li_input (+ 1 int_pos))))))


(define (hasStar? str_input) (if (number? (index-of (string->list str_input) #\*))
                                 #t #f))
(define (firstStarIndex str_input) (index-of (string->list str_input) #\*))



(define (doReplaceStar li_usedchars str_code int_pos)
  (map
   (lambda (eachChar)
     (replaceChar (string->list str_code) int_pos eachChar))
   li_usedchars))

(define (findAndReplaceStar li_usedchars str_code)
  (doReplaceStar li_usedchars str_code (firstStarIndex str_code)))

;(findAndReplaceStar usedCharsTest "abc*d*")


(define (repStar li_char nested_str)
  (flatten
  (map
   (lambda (each_elem)
     (cond
       [(list? each_elem)
        (repStar li_char each_elem)]
       [(and (string? each_elem) (hasStar? each_elem))
        (repStar li_char
                 (findAndReplaceStar li_char each_elem))]
       [(and (string? each_elem) (not (hasStar? each_elem))) (identity each_elem)]
       ))
   nested_str)))


(define (getListOfCodesFromCode str_code)
  (repStar usedChars (list str_code)))

;(getListOfCodesFromCode "abc*d")

;(repStar usedCharsTest (list "ab*c**"))

;(repStar usedCharsTest (list "abc"))

;(repStar usedCharsTest (list ""))



;;slut
