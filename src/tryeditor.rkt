#lang racket

(require racket/gui)

;hovedframen. alt i programmet skal gaa gennem den
(define hovedframe (new frame% [label "Simple Edit"]
                      [width 200]
                      [height 200]))

; Make a static text message in the frame
(define msg (new message% [parent hovedframe]
                          [label "nothing..."]))
 
(define loopresults (new message% [parent hovedframe]
                          [label "resultater af lookup"]))

;her er selve text omraadet
(define hovededitorcanvas (new editor-canvas% [parent hovedframe]))
(define hovedtextomraade (new text%))
(send hovededitorcanvas set-editor hovedtextomraade)



;en administrator der skal haandtere inputtet til hovedtextomraadet
(define hovedadmin (new editor-admin%))

(define mb (new menu-bar% [parent hovedframe]))
(define m-edit (new menu% [label "Edit"] [parent mb]))
(define m-font (new menu% [label "Font"] [parent mb]))
(define clickcontainer (new menu% [label "click"] [parent mb]))
(define clickleft (new menu-item% [label "Clickleft"]
                       [parent clickcontainer]
                       [callback (lambda (button event)
                        (send msg set-label "Left click"))]))
(define clickright (new menu-item% [label "Clickright"]
                        [parent clickcontainer]                       
                       [callback (lambda (button event)
                        (send msg set-label "Right click =================================================="))]))


(append-editor-operation-menu-items m-edit #f)
(append-editor-font-menu-items m-font)
(send hovedtextomraade set-max-undo-history 100)

(send hovedframe show #t)





;slut
