#lang racket

(require racket/fasl)

;arrayTextFiles:
(define array1RAW (file->string "../array30Source/array30_27489.txt"))
(define array1 (substring array1RAW 1 (string-length array1RAW)))

(define array2RAW (file->string "../array30Source/array30_ExtB.txt"))
(define array2 (substring array2RAW 1 (string-length array2RAW)))

(define array3RAW (file->string "../array30Source/array30_ExtCD.txt"))
(define array3 (substring array3RAW 1 (string-length array3RAW)))

;(string-length array3RAW)
;(substring array3RAW 0 10)
;(string-length array3)
;(substring array3 0 20)

;write a function that takes an array code string and return a nested list
;the inner list contain the code and then the character

(define (stringToList str_arraytext)
  (map (lambda (li_eachnested)
         (list (string-downcase (first li_eachnested))
               (second li_eachnested)
               (char->integer (string-ref (second li_eachnested) 0))))
  (map (lambda (str_eachline)
         (string-split str_eachline #px"\t"))
  (string-split str_arraytext #px"\n"))))

(define nestarray1 (stringToList array1))
;(length nestarray1)
;(first nestarray1)
;(second nestarray1)
;(last nestarray1)
(define nestarray2 (stringToList array2))
;(length nestarray2)
;(first nestarray2)
;(second nestarray2)
;(last nestarray2)
(define nestarray3 (stringToList array3))
;(length nestarray3)
;(first nestarray3)
;(second nestarray3)
(last nestarray3)

(define (verifyStructure li_li_arraynested)
  (equal? (length li_li_arraynested)
          (length
  (filter
   (lambda (li_eachlist)
     (and (equal? 3 (length li_eachlist))
          (< 0 (string-length (first li_eachlist)))
          (< 0 (string-length (second li_eachlist)))
          (integer? (list-ref li_eachlist 2))))
   li_li_arraynested))))

(verifyStructure nestarray1)
(verifyStructure nestarray2)
(verifyStructure nestarray3)


;write a function that combines the 3 nestedLists>
(define allArrayNested (append nestarray1 nestarray2 nestarray3))
(length allArrayNested)

;write a function that, for each character, creates a nested list of alle lines with the same character
(define (nestedListOfLines fun_listElemToUseAsKey li_li_allnestedarray)
  (let ([li_listElemsToUseAsKey
         (map
          (lambda (li_eachnestedlist)
            (fun_listElemToUseAsKey li_eachnestedlist))
          li_li_allnestedarray)])
    (map
     (lambda (str_eachwantedkey)
       (list str_eachwantedkey
             (filter
              (lambda (li_eacharrayline)
                (equal? str_eachwantedkey
                        (fun_listElemToUseAsKey li_eacharrayline)))
              li_li_allnestedarray)))
     li_listElemsToUseAsKey)))

;(define charsAsKeys (nestedListOfLines second allArrayNested))
;(define codesAsKeys (nestedListOfLines first allArrayNested))
;(call-with-output-file "../array30Maps/array30CharList.rktd"
;  (lambda (out) (s-exp->fasl charsAsKeys out))
;  #:exists 'replace)
;(call-with-output-file "../array30Maps/array30CodeList.rktd"
;  (lambda (out) (s-exp->fasl codesAsKeys out))
;  #:exists 'replace)

(define (load-data path)
  (call-with-input-file path fasl->s-exp))

(define readCharList (load-data "../array30Maps/array30CharList.rktd"))
(define readCodeList (load-data "../array30Maps/array30CodeList.rktd"))

;(equal? readCharList charsAsKeys)
;(equal? readCodeList codesAsKeys)
;"end of calculation"

(first readCharList)
(first readCodeList)

(length (set->list (list->set readCharList)))

;;write a function that removes the unnessasary information from the nested lists

;write a function that creates a hashmap from the nested lists




;slut
