;;;;tests.lisp

(in-package :formulador-lite)



(defvar test-cases1
  '("1+2"
    "1-2"
    "1*2"
    "1=2"
    "1/2"
    "1^2"
    "[1+2]"
    "[1-2]"
    "[1*2]"
    "[1/2]"
    "[1^2]"))

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
