#lang racket
(require racket/gui)

;use command field to to generate input for the main text field
(define (getResultOfCommandCode StrCommandCode)
  (identity "hejDer")) 

;***************************** Gui Code **********************************

;first frame of the program
(define firstFrame (new frame% [label "An editor that allow you to input commands"]
                      [width 500]
                      [height 500]))

;the commands you type should be vissible in this message control
(define commandField (new message% [parent firstFrame]
                          [label ""]))

(define (writeToCommandField userString)
  (send commandField set-label userString))

(define preliminaryString "")

(define (addStringToCmdField newString)
  (writeToCommandField (string-append (send commandField get-label) newString)))

(define (handleCmdFieldInput keyEvent currentClass)
  (if (equal? (send keyEvent get-key-code) (integer->char 32)) ;space character     
      (begin (send currentClass insert (getResultOfCommandCode (send commandField get-label)))
             (writeToCommandField "")
             (send currentClass insert #"\backspace")
             )
      (begin (addStringToCmdField (string (send keyEvent get-key-code)))
             (send currentClass insert #"\backspace"))))

;the text area
(define mainCanvas (new editor-canvas% [parent firstFrame]))

(define mainTextArea (new (class text%
(define/override (on-char e)
  (when (and (not (send e get-caps-down))
             (char? (send e get-key-code)))
    (handleCmdFieldInput e this))
    ;(addStringToCmdField (string (send e get-key-code)))
    ;(send this insert #"\backspace")) ;this gives an error in console, but it works
(super on-char e))              
(super-new))))

(send mainCanvas set-editor mainTextArea)

;A menu bar for ordinary texteditor functionality
(define manubar (new menu-bar% [parent firstFrame]))
(define editDropdown (new menu% [label "Edit"] [parent manubar]))
(define fontDropdown (new menu% [label "Font"] [parent manubar]))

(append-editor-operation-menu-items editDropdown #f)
(append-editor-font-menu-items fontDropdown)
(send mainTextArea set-max-undo-history 100)

(send firstFrame show #t)
