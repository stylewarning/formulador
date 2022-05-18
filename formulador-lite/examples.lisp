;;;; formulador-lite/examples.lisp
;;;;
;;;; Copyright (c) 2021 Izaak Walton

(in-package #:formulador-lite)

;;; with the formulador-lite interpreter:

;FORMULADOR-LITE> (start-drawing)
;x/y
;#<CANVAS 
;+---+
;| X |
;|---|
;| Y |
;+---+
					;with 3 defined regions>
(defun pythagorean-theorem ()
  (simple-draw "[a^2]+[b^2]=[c^2]"))

;#<CANVAS 
;+------------+
;| 2    2    2|
;|A  + B  = C |
;+------------+
					;with 15 defined regions>
(defun harmonic-test ()
  (simple-draw "1/[[1/h]+[1/a]+[1/r]+[1/m]+[1/o]+[1/n]+[1/i]+[1/c]]"))

;#<CANVAS 
;+-----------------------------------------------+
;|                       1                       |
;|-----------------------------------------------|
;|  1     1     1     1     1     1     1     1  |
;| --- + --- + --- + --- + --- + --- + --- + --- |
;|  H     A     R     M     O     N     I     C  |
;+-----------------------------------------------+
;with 35 defined regions>
;stop
;Drawing terminated
;NIL
;FORMULADOR-LITE> 

;;; Without the interpreter:

;FORMULADOR-LITE> (simple-draw "x/y") ;or (simple-draw "[x/y]")
;#<FORMULADOR:CANVAS 
;+-----+
;| |x| |
;|-----|
;| |y| |
;+-----+
;with 3 defined regions>


;(simple-draw "[[x/y]+[w/[u+v]]]/[[p+q]/[r+[1/s]]]")

;;;;------------------------------------------------------------------------

