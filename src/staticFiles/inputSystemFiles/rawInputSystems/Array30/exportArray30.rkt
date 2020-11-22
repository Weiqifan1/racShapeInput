#lang racket

(require racket/fasl)


(provide array30CharHash array30CodeHash)

(define (load-data path)
  (call-with-input-file path fasl->s-exp))

(define array30CharHash (load-data "array30Maps/array30CharHash.rktd"))
(define array30CodeHash (load-data "array30Maps/array30CodeHash.rktd"))

;(hash-ref array30CharHash "人")
;(hash-ref array30CodeHash "h")

;(define readCharHash (load-data "../array30Maps/array30CharHash.rktd"))
;(define readCodeHash (load-data "../array30Maps/array30CodeHash.rktd"))



;slut
