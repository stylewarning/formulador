;;;; examples.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith
;;;;
;;;; This file contains some examples of typeset formulas.

(in-package #:formulador)

(defun pow (a b)
  (script-box a :superscript b))


;;; > (draw *chudnovsky*)
;;;
;;; +------------------------------------------------------------------------+
;;; |                    ∞                                                   |
;;; |             3/2   ===                                                 k|
;;; | 1     640320      \     (6k)! (545140134k + 13591409)  /       1     \ |
;;; |--- = -----------   >   ------------------------------- | - --------- | |
;;; | π        12       /                        3           |          3  | |
;;; |                   ===            (3k)! (k!)            \    640320   / |
;;; |                  k = 0                                                 |
;;; +------------------------------------------------------------------------+

(defparameter *chudnovsky*
  (tape
   (frac-box "1" "π")
   "="
   (frac-box (pow "640320" "3/2")
             "12")
   (limits-box +sigma+
               :above "∞"
               :below "k = 0")
   (frac-box (tape "(6k)!" "(545140134k + 13591409)")
             (tape "(3k)!" (pow "(k!)" "3")))
   (pow (parens-box
         (tape "-" (frac-box "1"
                             (pow "640320" "3"))))
        "k")))
