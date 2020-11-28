#lang racket

(require "deserializeStaticFiles.rkt")

(hash-ref cangjie5Code "bmi")

(define (handleInput str_userInput)
  ;(identity str_userInput))
  (define inputSystemResult (if (hash-has-key? cangjie5Code (string-trim str_userInput))
      (hash-ref cangjie5Code (string-trim str_userInput))
      (identity "")))
  (if (list? inputSystemResult)
      (first inputSystemResult)
      (identity inputSystemResult)))
  
  

  
;should return a string
(define (displayListAndInputContent li_previousResults str_input)
  (apply string-append
  (append li_previousResults
  (list
  (handleInput str_input)))))

;returns a list of inputsystem hits. It is empty if there are none
(define (getCandidates str_userInput)
  (define inputSystemResult (if (hash-has-key? cangjie5Code (string-trim str_userInput))
      (hash-ref cangjie5Code (string-trim str_userInput))
      (identity '())))
  (identity inputSystemResult))

;console program tager en string fra brugeren og skriver en string som output.
;outputtet er resultatet af funktionen "processConsoleInput a" .
;hvis stringen fra brugeren ikke er "quit", vil brugeren blive spurgt igen
(define (iter lst)
  (display "input: ")
  (define userInput (read-line (current-input-port) 'any))
  (printf "\n")
      (cond [(equal? userInput "quit") "the program is over"]
            [else           
             (define previousResults (displayListAndInputContent lst userInput))
             (display previousResults)
             (display "\n")
             (iter (list previousResults))]))

;skriv det program jeg kalder
(define run (iter '()))






;slut
