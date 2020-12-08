#lang racket
;https://alex-hhh.github.io/2020/05/markdown-view.html
;https://cs.brown.edu/courses/cs173/2008/Manual/gui/doc-index.html#alpha:C
;https://www.cs.utah.edu/plt/snapshots/current/doc/framework/index.html
;https://docs.racket-lang.org/gui/Editor_Functions.html
;http://docs.racket-lang.org/gui/Widget_Gallery.html?q=gui
;http://rosettacode.org/wiki/GUI_enabling/disabling_of_controls#Racket

(require racket/gui)

;hovedframen. alt i programmet skal gaa gennem den
(define hovedframe (new frame% [label "Simple Edit"]
                      [width 500]
                      [height 500]))

; Make a static text message in the frame
(define msg (new message% [parent hovedframe]
                          [label "nothing..."]))
(define loopresults (new message% [parent hovedframe]
                          [label "resultater af lookup"]))

;her er selve text omraadet
(define hovededitorcanvas (new editor-canvas% [parent hovedframe]))
(define hovedtextomraade (new text%))
(send hovededitorcanvas set-editor hovedtextomraade)
;;;

;(define writeToMessage 
;  (send msg set-label "her er text"))


;;;;
;en administrator der skal haandtere inputtet til hovedtextomraadet
;(define hovedadmin (new editor-admin%))
(define mb (new menu-bar% [parent hovedframe]))
(define m-edit (new menu% [label "Edit"] [parent mb]))
(define m-font (new menu% [label "Font"] [parent mb]))
(define clickcontainer (new menu% [label "click"] [parent mb]))
(define clickleft (new menu-item% [label "Clickleft"]
                       [parent clickcontainer]
                       [callback (lambda (button event)
                        (send msg set-label "Left click"))]))
(define showText (new menu-item% [label "showtext"]
                        [parent clickcontainer]                       
                       [callback (lambda (button event)
                        (send msg set-label "Right click"))]))


(append-editor-operation-menu-items m-edit #f)
(append-editor-font-menu-items m-font)
(send hovedtextomraade set-max-undo-history 100)

(send hovedframe show #t)



;(define chrtextclass%
;  (class text%
;    (super-new)
;    (define/override (on-focus on?)
;      (when on? (printf "~a\n" (send this get-label))))))

;kan bruges til at indsaette text i frame
;(define lorem-ipsum #<<EOS
;Add text paragraphs here
;EOS
;  )
;(send hovedtextomraade insert (make-object string-snip% lorem-ipsum))

;(define skrivMedP (new keymap%))
;(add-text-keymap-functions skrivMedP)
;(send skrivMedP add-function "writeToMessage" writeToMessage)
;(send skrivMedP map-function "c:p" "writeToMessage")
;(send hovedtextomraade set-keymap skrivMedP)



;slut

