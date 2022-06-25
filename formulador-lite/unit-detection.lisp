;;;; formulador-lite/unit-detection.lisp
;;;;
;;;; Copyright (c) 2021 Izaak Walton

(in-package :formulador-lite)

;;;Formula unit detection:

;;;Bracket and Block Detection

(defun detect-brack (lexed-line)
  "Detects the beginning of a bracketed block."
  (eq (car (first lexed-line)) ':left-brack))

(defun detect-end-brack (lexed-line)
  "Detects the end of a bracketed block."
  (eq (car (first lexed-line)) ':right-brack))

(defun brack-length-backend (lexed-line counter)
  "Determines the number of elements before the end of a bracketed block."
  (cond ((detect-end-brack lexed-line)
         counter)
	(t (brack-length-backend (rest lexed-line) (+ counter 1)))))

(defun brack-length (lexed-line)
  (brack-length-backend lexed-line 0))

(defun detect-block (blocked-line)
  "Detects whether there is a unit."
  (eq (car (first blocked-line)) ':block))

;;; Parenthesis Detection

(defun detect-paren (lexed-line)
  "Detects whether there is a parenthesis group."
  (eq (car (first lexed-line)) ':left-paren))

(defun detect-end-paren (lexed-line)
  "Detects the closing parenthesis for a parenthesis group."
  (eq (car (first lexed-line)) ':right-paren))

;;; Exponent Detection

(defun detect-exp (lexed-line)
  "Detects whether there is an exponent."
  (eq (car (second lexed-line)) ':exponent))

;;; Fraction Detection

(defun detect-frac (lexed-line)
  "Detects whether there is a fraction."
  (eq (car (second lexed-line)) ':frac))

;;; Infix operator chain Detection

(defun detect-infix-chain (lexed-line)
  "Detects whether the first object is a member of an add/subtract/multiply chain."
  (eq (car (second lexed-line)) ':infix-oper))

(defun detect-infix-chain-end (lexed-line)
  "Detects whether the asm chain should end."
  (if (not (member (car (second lexed-line)) '(:number :symbol :block :infix-oper)))
      (if (member (car (first lexed-line)) '(:number :symbol :block :infix-oper))
	  nil
	  t)))
      

(defun detect-infix-chain-length-backend (lexed-line counter)
  (if (detect-infix-chain-end lexed-line)
      counter
      (detect-infix-chain-length-backend (rest lexed-line) (+ counter 1))))

(defun detect-infix-chain-length (lexed-line)
  (detect-infix-chain-length-backend lexed-line 1))
	  
;;;;------------------------------------------------------------------------
