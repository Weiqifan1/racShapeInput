#lang racket
(require racket/gui)

;CURRENT FUNCTION
(define (commandCodeControllerStrToList StrCommandCode)
  (list "1 first info About First" "2 second info About Second" "3 third info about third"))

;(define (writeCandidateListToFrame li_candidateList currentClass)
;  ())

;use command field to to generate input for the main text field
(define (getResultOfCommandCode StrCommandCode currentClass)
  (string-append  "1 " StrCommandCode)) 

;write a function that takes list of commandCode results
;* it writes the list to the candidate frame
;* it asks for a single number keystroke that must be no greater than the list size (max 10)
;* it then writes the chinese string (the result) to the editor textarea.
;* it then clears the candidate frame



;***************************** Gui Code **********************************

;first frame of the program
(define firstFrame (new frame% [label "An editor that allow you to input commands"]
                      [width 500]
                      [height 500]))

;the commands you type should be vissible in this message control
(define commandField (new message% [parent firstFrame] ;[parent firstFrame]
                          [label ""]))

(define (writeToCommandField userString)
  (send commandField set-label userString))

(define preliminaryString "")

;(define (addStringToCmdField strInput)
  ;(when (and (char? eventKeyCode)
   ;          (char-graphic? eventKeyCode))
;  (writeToCommandField strInput))

(define (updatedCommandFieldValue charOrKeyword)
  (let ([currentCommandField (send commandField get-label)])
    (if (and (char? charOrKeyword)
             (char-graphic? charOrKeyword))
        (string-append currentCommandField (string charOrKeyword))
        (identity currentCommandField)
        )))

(define (handleCmdFieldInput keyEvent currentClass)
  (let* ([charOrKeyword (send keyEvent get-key-code)]
         [updatedCmd (updatedCommandFieldValue charOrKeyword)]
         [cmdResult (getResultOfCommandCode updatedCmd currentClass)])
  (if (equal? (send keyEvent get-key-code) (integer->char 32)) ;space character     
      (begin (send currentClass insert cmdResult)
             (writeToCommandField "")
             (send currentClass insert #"\backspace")
             )
      (begin (writeToCommandField updatedCmd)
             (send currentClass insert #"\backspace")))))

;THE CANDIDATE AREA (ON THE TOP OF THE FRAME)
(define candidateCanvas (new editor-canvas% [parent firstFrame]))
(define candidateTextArea (new text%))
(send candidateCanvas set-editor candidateTextArea)

;the WRITING area
(define editorCanvas (new editor-canvas% [parent firstFrame]))

(define editorTextArea (new (class text%
(define/override (on-char e)
  (when (and (not (send e get-caps-down))
             (char? (send e get-key-code)))
    (handleCmdFieldInput e this))
    ;(addStringToCmdField (string (send e get-key-code)))
    ;(send this insert #"\backspace")) ;this gives an error in console, but it works
(super on-char e))              
(super-new))))

(send editorCanvas set-editor editorTextArea)

;A menu bar for ordinary texteditor functionality
(define manubar (new menu-bar% [parent firstFrame]))
(define editDropdown (new menu% [label "Edit"] [parent manubar]))
(define fontDropdown (new menu% [label "Font"] [parent manubar]))

(append-editor-operation-menu-items editDropdown #f)
(append-editor-font-menu-items fontDropdown)
(send editorTextArea set-max-undo-history 100)

(send firstFrame show #t)
