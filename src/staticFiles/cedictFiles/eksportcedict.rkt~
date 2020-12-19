#lang racket

(require racket/fasl)


(provide tcedict scedict)

(define (load-data path)
  (call-with-input-file path fasl->s-exp))

(define tcedict (load-data "cedictMaps/cedictTradSerial.rktd"))
(define scedict (load-data "cedictMaps/cedictSimpSerial.rktd"))

;(hash-ref tcedict "髮")
;(hash-ref scedict "发")



;slut
