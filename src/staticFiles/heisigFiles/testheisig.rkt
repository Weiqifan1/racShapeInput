#lang racket



(require racket/fasl)

(define (load-data path)
  (call-with-input-file path fasl->s-exp))

(define theisig (load-data "heisigMaps/heisigTradSerial.rktd"))
(define sheisig (load-data "heisigMaps/heisigSimpSerial.rktd"))

;traditionel heisig eksempel
(hash-ref theisig "診")

;simplificeret heisig eksempel
(hash-ref sheisig "长")



;slut
