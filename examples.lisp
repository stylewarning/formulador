;;;; examples.lisp
;;;;
;;;; Copyright (c) 2014-2018 Robert Smith

;;; This file contains some examples of typeset formulas.

(defpackage #:formulador-examples
  (:use #:cl #:formulador)
  (:export #:pow
           #:*chudnovsky*
           #:*gauss-law*
           #:*thermo*
           #:subscript-test
           #:draw-all))

(in-package #:formulador-examples)

(defun pow (a b)
  (script-box a :superscript b))

(defun sub (a b)
  (script-box a :subscript b))


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
          (ell (box (code-char #x2113)))
          (congruent (box (code-char #x2245))))
      (tape
       (glue (script-box (tilde (box #\N))
                         :subscript (script-box phi
                                                :subscript ell
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
                                                                         :subscript ell
                                                                         :superscript (box #\E)))
                                                            (box #\*))
                                                       (box #\G)))
                          (parens-box (box #\0)))))))))

(defun subscript-test (n)
  (assert (and (integerp n)
               (not (minusp n))))
  (labels ((frob (n)
             (parens-box
              (if (zerop n)
                  (box "*")
                  (let ((next (frob (1- n))))
                    (script-box next :superscript next
                                     :subscript next))))))
    (frob n)))

(defparameter *tb-22-4-ex1*
  ;; Slightly modified example from the TUGBoat Vol 22 No 4.
  (tape
   (box "X")
   (box "=")
   ;; Use of CLAP
   (limits-box +sigma+
               :below (clap (box "1 ≤ i ≤ n")))
   (sub (box "X") (box "i"))
   (box "=")
   ;; No use of CLAP
   (limits-box +sigma+
               :below (box "1 ≤ i ≤ n"))
   (sub (box "X") (box "i"))))

(defun draw-all ()
  (flet ((demo (string c)
           (write-line string)
           (princ (draw c))
           (terpri)
           (terpri)))
    (demo "Chudnovsky formula:" *chudnovsky*)
    (demo "Gauss's law:" *gauss-law*)
    (demo "Random thing from thermo:" *thermo*)
    (demo "Nested subscripts:" (subscript-test 2))
    (demo "mathclap demo:" *tb-22-4-ex1*)
    ':done))
