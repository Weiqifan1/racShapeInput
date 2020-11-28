#lang racket

(require "deserializeStaticFiles.rkt")

(hash-ref cangjie5Code "bmi")
  

;returns a list of inputsystem hits. It is empty if there are none
(define (getCandidates str_userInput)
  (define inputSystemResult (if (hash-has-key? cangjie5Code (string-trim str_userInput))
      (hash-ref cangjie5Code (string-trim str_userInput))
      (identity '())))
  (identity inputSystemResult))

;get a list of possibleCandidates and ask the user to choose among them
;if the list is empty, it retuns an empty string
(define (chooseCandidate li_candidates)
  (cond [(empty? li_candidates)
         (display "no hits\n")
         (identity "")]
        [else
         (display (string-append "choseOne: "
                   (apply string-append (map
                   (lambda (eachCandidate) (identity eachCandidate))
                   li_candidates))))
         (display "\n")
         (define userChooseResult (read-line (current-input-port) 'any))
         (define chosenInput (- (string->number userChooseResult) 1))
         (vector-ref (list->vector li_candidates) chosenInput)
         ]))
 

;console program tager en string fra brugeren og skriver en string som output.
;outputtet er resultatet af funktionen "processConsoleInput a" .
;hvis stringen fra brugeren ikke er "quit", vil brugeren blive spurgt igen
(define (iter lst)
  (display "input: ")
  (define userInput (read-line (current-input-port) 'any))
  (printf "\n")
      (cond [(equal? userInput "quit") "the program is over"]
            [else
             (define candidates (getCandidates userInput))
             (define pickACandidate (chooseCandidate candidates))             
             (define resultOfPick (string-append (apply string-append lst) pickACandidate))
             (display resultOfPick)           
             (display "\n")
             (iter (list resultOfPick))]))

;skriv det program jeg kalder
(define run (iter '()))






;slut
