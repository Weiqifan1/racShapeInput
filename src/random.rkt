#lang racket
(require racket/gui)

;use command field to to generate input for the main text field
;this should return a nested list of strings, each list is a lookup result
(define (getResultOfCommandCode StrCommandCode StrOldCmdField Str_inputSystemField currentClass)
  (list (list (string-append  "1a " StrCommandCode)
              (string-append  "1b " StrCommandCode))
        (list (string-append  "2a " StrCommandCode)
              (string-append  "2b " StrCommandCode))
        ))

;expects a list from the above function, and returns a string suitable for
;being printed to the candiate field
(define (createDisplayStringFromCommandResult li_li_str_commandResult)
  (apply string-append
  (map
   (lambda (eachList)
     (string-append 
     (apply string-append
     (map
      (lambda (eachStr)
        (string-append eachStr " "))
      eachList)) "\n"))
   li_li_str_commandResult)))

;function that takes a command result and returns the string to be written to the editor window
(define (elemToWriteFromResult li_li_str_commandResult str_UpdatedCommand)
  (if (and (< 0 (length li_li_str_commandResult))
           (< 0 (length (first li_li_str_commandResult))))
      (first (first li_li_str_commandResult))
      (identity "")))

;(substring oldCmdValue 0 (- (string-length oldCmdValue) 1))

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

;currentInputSystemField
(define inputssytemField (new message% [parent firstFrame] [label "C"]))

(define (isSingleCapitalLetter str_input)
  (if (and (equal? 1 (string-length str_input))
           (< 64 (char->integer (string-ref str_input 0))) ;at least A
           (> 91 (char->integer (string-ref str_input 0))));not greater than Z
      #t
      #f))

(define (updateInputsystmField str_commandFieldContent)
  (when (isSingleCapitalLetter str_commandFieldContent)
      (send inputssytemField set-label str_commandFieldContent)))

;the commands you type should be vissible in this message control
(define commandField (new message% [parent firstFrame] ;[parent firstFrame]
                          [label ""]))

(define (writeToCommandField userString)
  (send commandField set-label userString))

(define (updatedCommandFieldValue charOrKeyword oldCmdValue)
    (if (and (char? charOrKeyword)
             (char-graphic? charOrKeyword))
        (string-append oldCmdValue (string charOrKeyword))
        (if (and (equal? charOrKeyword (integer->char 8))
                 (< 0 (string-length oldCmdValue)))
            (substring oldCmdValue 0 (- (string-length oldCmdValue) 1))          
            (identity oldCmdValue))))

(define (handleCmdFieldInput keyEvent currentClass)
  (let* ([inputSystemField (send inputssytemField get-label)]
         [oldCmdField (send commandField get-label)]
         [charOrKeyword (send keyEvent get-key-code)]
         [updatedCmd (updatedCommandFieldValue charOrKeyword oldCmdField)]
         [cmdResult (getResultOfCommandCode updatedCmd oldCmdField inputSystemField currentClass)]
         [cmdResultStringifyed (createDisplayStringFromCommandResult cmdResult)])
  (if (equal? charOrKeyword (integer->char 32)) ;space character     
      (when (not (equal? "" (send commandField get-label)))
          (if (isSingleCapitalLetter updatedCmd)
              (begin
                (updateInputsystmField updatedCmd)
                (writeToCommandField "")
                (send currentClass insert #"\backspace"))
              (begin
                (send currentClass insert (elemToWriteFromResult cmdResult updatedCmd))
                (writeToCommandField "")
                (send currentClass insert #"\backspace"))))
      (begin (send candidateTextArea erase)
             (send candidateTextArea insert cmdResultStringifyed)
             (writeToCommandField updatedCmd)
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
             (char? (send e get-key-code))
             (or (char-graphic? (send e get-key-code))
                 (equal? (send e get-key-code) (integer->char 32)) ;space
                 (and (equal? (send e get-key-code) (integer->char 8)) ;backspace
                      (< 0 (string-length (send commandField get-label))))))
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
