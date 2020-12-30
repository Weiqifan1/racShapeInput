#lang racket
(require racket/fasl)

(provide cedictTrad cedictSimp
         heisigTrad heisigSimp
         array30Char array30Code
         cangjie5Char cangjie5Code inputhash alternative)
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
(define dayi4Char (load-data "staticFiles/inputSystemFiles/rawInputSystems/Dayi4/Dayi4Maps/dayi4CharHash.rktd"))
(define dayi4Code (load-data "staticFiles/inputSystemFiles/rawInputSystems/Dayi4/Dayi4Maps/dayi4CodeHash.rktd"))


(define dayi3Char (load-data "staticFiles/inputSystemFiles/rawInputSystems/Dayi63/Dayi3Maps/dayi3CharHash.rktd"))
(define dayi3Code (load-data "staticFiles/inputSystemFiles/rawInputSystems/Dayi63/Dayi3Maps/dayi3CodeHash.rktd"))


(define wubiChar (load-data "staticFiles/inputSystemFiles/rawInputSystems/Wubi/WubiMaps/wubiCharHash.rktd"))
(define wubiCode (load-data "staticFiles/inputSystemFiles/rawInputSystems/Wubi/WubiMaps/wubiCodeHash.rktd"))


(define wubihuaChar (load-data "staticFiles/inputSystemFiles/rawInputSystems/Wubihua/WubihuaMaps/wubihuaCharHash.rktd"))
(define wubihuaCode (load-data "staticFiles/inputSystemFiles/rawInputSystems/Wubihua/WubihuaMaps/wubihuaCodeHash.rktd"))


(define zhengmaChar (load-data "staticFiles/inputSystemFiles/rawInputSystems/Zhengma/ZhengmaMaps/zhengmaCharHash.rktd"))
(define zhengmaCode (load-data "staticFiles/inputSystemFiles/rawInputSystems/Zhengma/ZhengmaMaps/zhengmaCodeHash.rktd"))

(define inputhash
  (hash "A" (list array30Code array30Char)
        "C" (list cangjie5Code cangjie5Char)
        "D" (list dayi3Code dayi3Char)
        "E" (list dayi4Code dayi4Char)
        "W" (list wubiCode wubiChar)
        "V" (list wubihuaCode wubihuaChar)
        "Z" (list zhengmaCode zhengmaChar)))
(define alternative
  (list cangjie5Code cangjie5Char))

;(hash-ref array30Char "方")
;(hash-ref array30Code "yhs")
;(hash-ref cangjie5Char "方")
;(hash-ref cangjie5Code "yhs")

;2020-11-22
;;;;;;;;;;;;;nu har jeg importeret de noedvendige filer.
;;; nu skal jeg enten lave hashmaps til de oevrige systemer eller se paa grafik. 





;slut
