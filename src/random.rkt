#lang racket
(require racket/gui)

;first frame of the program
(define firstFrame (new frame% [label "An editor that allow you to input commands"]
                      [width 500]
                      [height 500]))

;the commands you type should be vissible in this message control
(define commandField (new message% [parent firstFrame]
                          [label "no commands yet..."]))

(define (writeToCommandField character)
  (send commandField set-label character))

;the text area
(define mainCanvas (new editor-canvas% [parent firstFrame]))

(define mainTextArea (new (class text%
(define/override (on-char e)
  (when (send e get-caps-down)
(writeToCommandField e))
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