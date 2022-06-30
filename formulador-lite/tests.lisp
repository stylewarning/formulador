;;;; tests.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(ql:quickload :fiasco)

(fiasco:define-test-package #:formulador-tests
    (:use #:formulador-lite))

(in-package #:formulador-tests)

(defun canvas-eq (canvas1 canvas2)
  "Compares two canvases for equivalence."
  (string-equal (write-to-string canvas1) (write-to-string canvas2)))

					; (run-package-tests getting the wrong number of arguments....)
(deftest solo-boxes ()
  (is (canvas-eq (simple-draw "1") (formulador:draw (formulador:box " 1 "))))
  (is (canvas-eq (simple-draw "cabbage") (formulador:draw (formulador:box " cabbage "))))
  (is (canvas-eq (simple-draw "it's working") (formulador:draw (formulador:box " it's working ")))))

;(deftest infix-operations ()
 ; (is (canvas-eq (simple-draw "1+2") (formulador:draw (formulador:glue
  ;                                                        (formulador:box " 1 ")
   ;                                                       (formulador:box " + ")
    ;                                                      (formulador:box " 2 ")))))
 ; (is (canvas-eq (simple-draw "3-5") (formulador:draw (formulador:glue
  ;                                                        (formulador:box " 3 ")
   ;                                                       (formulador:box " - ")
    ;                                                      (formulador:box " 5 ")))))
 ; (is (canvas-eq (simple-draw "r*s") (formulador:draw (formulador:glue
   ;                                                       (formulador:box " r" )
  ;                                                        (formulador:box " * ")
   ;                                                       (formulador:box " s ")))))
 ; (is (eq (simple-draw "hungry=me") (formulador:draw (formulador:glue
  ;                                                              (formulador:box " hungry ")
   ;                                                             (formulador:box " = ")
    ;                                                            (formulador:box " me "))))))

;(deftest exponents 

(defvar test-cases1
  '(("1+2" (formulador:draw (formulador:box "1") (formulador:box "+") (formulador:box "2")))
    ("1-2" (formulador:draw (formulador:box "1") (formulador:box "1") (formulador:box "2")))
    ("1*2")
    ("1=2")
    ("1/2")
    ("1^2")
    ("[1+2]")
    ("[1-2]")
    ("[1*2]")
    ("[1/2]")
    ("[1^2]")))

(defvar test-cases2
  '("(1+2)"
    "(1-2)"
    "(1*2)"
    "(1=2)"
    "(1/2)"
    "(1^2)"      ;bug
    "[(1+2)]"    ;bug
    "[(1/2)]"
    "[(1^2)]"    ;bug
    "([1+2])"
    "([1/2])"
    "([1^2])"))  ;bug

(defvar test-cases3 
  '("[1+2]+[2+3]"
    "[1+2]/[2+3]"
    "[1+2]^[2+3]"
    "[1/2]+[2/3]"
    "[1/2]/[2/3]"
    "[1/2]^[2/3]"
    "[1^2]+[2^3]"
    "[1^2]^[2^3]"
    "[1^2]/[2^3]"))

(defvar test-cases4
  '("1/[[1/h]+[1/a]+[1/r]+[1/m]+[1/o]+[1/n]+[1/i]+[1/c]]"
    "[[x/y]+[w/[u+v]]]/[[p+q]/[r+[1/s]]]"))
    
(defun run-tests (test-list)
  (cond ((null test-list) nil)
	(t (progn (format t "~a" (simple-draw (first test-list)))
		  (run-tests (rest test-list))))))

(defun run-all-tests (test-list)
  (run-tests test-cases1)
  (run-tests test-cases2)
  (run-tests test-cases3)
  (run-tests test-cases4))
