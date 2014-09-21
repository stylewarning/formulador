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
;;; |         _______    ∞                                                   |
;;; |        |      3   ===                                                 k|
;;; | 1     \|640320    \     (6k)! (545140134k + 13591409)  /       1     \ |
;;; |--- = -----------   >   ------------------------------- | - --------- | |
;;; | π        12       /                        3           |          3  | |
;;; |                   ===            (3k)! (k!)            \    640320   / |
;;; |                  k = 0                                                 |
;;; +------------------------------------------------------------------------+


(defparameter *chudnovsky*
  (let ((pi-letter (box (code-char #x3C0))))
    (tape
     (frac-box (box "1") pi-letter)
     (box "=")
     (frac-box (sqrt-box (pow (box "640320")
                              (box "3")))
               (box "12"))
     (limits-box +sigma+
                 :above (box "∞")
                 :below (box "k = 0"))
     (frac-box (tape (box "(6k)!")
                     (box "(545140134k + 13591409)"))
               (tape (box "(3k)!")
                     (pow (box "(k!)")
                          (box "3"))))
     (pow (parens-box
           (tape (box "-") (frac-box (box "1")
                                     (pow (box "640320")
                                          (box "3")))))
          (box "k")))))

(defparameter *gauss-law*
  (let ((rho (box (code-char #x03C1)))
        (S (box "S"))
        (dS (glue +partial+ (box "S"))))
    (tape (limits-box +triple-integral+ :below S) rho (box "dV")
          (box "=")
          (limits-box +double-integral+ :below dS) (glue (box "E")
                                                         +center-dot+
                                                         (box "dA")))))

(defparameter *thermo*
  (flet ((tilde (box) (limits-box box :above (box #\~))))
    (let ((phi (box (code-char #x3D5)))
          (theta (box (code-char #x398)))
          (congruent (box (code-char #x2245))))
      (tape
       (glue (script-box (tilde (box #\N))
                         :subscript (script-box phi
                                                :subscript (box #\l)
                                                :superscript (box "G/E")))
             (pow theta (box #\k))
             (parens-box (glue (script-box (box #\I)
                                           :subscript (glue (pow (box #\i)
                                                                 (box #\*))
                                                            (box #\G)))
                               (parens-box (box #\0)))))
       
       congruent
       
       (glue
        (pow theta (box #\k))
        (parens-box (glue (script-box (box #\I)
                                      :subscript (glue (pow (parens-box
                                                             (script-box phi
                                                                         :subscript (box #\l)
                                                                         :superscript (box #\E)))
                                                            (box #\*))
                                                       (box #\G)))
                          (parens-box (box #\0)))))))))
