#lang racket

(require racket/fasl)


(provide theisig sheisig)

(define (load-data path)
  (call-with-input-file path fasl->s-exp))

(define theisig (load-data "heisigMaps/heisigTradSerial.rktd"))
(define sheisig (load-data "heisigMaps/heisigSimpSerial.rktd"))

;(hash-ref theisig "髮")
;(hash-ref sheisig "发")

;;slut
