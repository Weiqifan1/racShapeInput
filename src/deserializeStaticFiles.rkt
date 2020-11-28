#lang racket
(require racket/fasl)

(provide cedictTrad cedictSimp
         heisigTrad heisigSimp
         array30Char array30Code
         cangjie5Char cangjie5Code)
;proev at provide

(define (load-data path)
  (call-with-input-file path fasl->s-exp))


;(define array30CodeHash (load-data "array30Maps/array30CodeHash.rktd"))
(define cedictSimp (load-data "staticFiles/cedictFiles/cedictMaps/cedictSimpSerial.rktd"))
(define cedictTrad (load-data "staticFiles/cedictFiles/cedictMaps/cedictTradSerial.rktd"))

;(hash-ref cedictTrad "方")
;(hash-ref cedictSimp "方")

(define heisigTrad (load-data "staticFiles/heisigFiles/heisigMaps/heisigTradSerial.rktd"))
(define heisigSimp (load-data "staticFiles/heisigFiles/heisigMaps/heisigSimpSerial.rktd"))

;(hash-ref heisigTrad "方")
;(hash-ref heisigSimp "方")

(define array30Char (load-data "staticFiles/inputSystemFiles/rawInputSystems/Array30/array30Maps/array30CharHash.rktd"))
(define array30Code (load-data "staticFiles/inputSystemFiles/rawInputSystems/Array30/array30Maps/array30CodeHash.rktd"))
(define cangjie5Char (load-data "staticFiles/inputSystemFiles/rawInputSystems/Cangjie/Cangjie5Maps/cangjie5CharHash.rktd"))
(define cangjie5Code (load-data "staticFiles/inputSystemFiles/rawInputSystems/Cangjie/Cangjie5Maps/cangjie5CodeHash.rktd"))

;(hash-ref array30Char "方")
;(hash-ref array30Code "yhs")
;(hash-ref cangjie5Char "方")
;(hash-ref cangjie5Code "yhs")

;2020-11-22
;;;;;;;;;;;;;nu har jeg importeret de noedvendige filer.
;;; nu skal jeg enten lave hashmaps til de oevrige systemer eller se paa grafik. 





;slut
