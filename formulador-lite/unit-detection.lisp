;;;;formulador-lite/unit-detection.lisp
;;;;
;;;;Copyright (c) 2021 Izaak Walton

(in-package :formulador-lite)
;;;;------------------------------------------------------------------------
;;;;Formula unit detection:
;;;;------------------------------------------------------------------------

;;;;------------------------------------------------------------------------
;;;;Bracket and Block Detection
;;;;------------------------------------------------------------------------
(defun detect-brack (lexed-line)
  "Detects the beginning of a bracketed block."
  (equal (car (first lexed-line)) ':left-brack))

(defun brack-length (lexed-line counter)
  "Determines the number of elements before the end of a bracketed block."
  (cond ((detect-end-brack lexed-line) counter)
	(t (brack-length (rest lexed-line) (+ counter 1)))))

(defun detect-end-brack (lexed-line)
  "Detects the end of a bracketed block."
  (equal (car (first lexed-line)) ':right-brack))

(defun detect-block (blocked-line)
  "Detects whether there is a unit."
  (equal (car (first blocked-line)) ':block))

;;;;------------------------------------------------------------------------
;;;;Parenthesis Detection
;;;;------------------------------------------------------------------------
(defun detect-paren (lexed-line)
  "Detects whether there is a parenthesis group."
  (equal (car (first lexed-line)) ':left-paren))

(defun detect-end-paren (lexed-line)
  "Detects the closing parenthesis for a parenthesis group."
  (equal (car (first lexed-line)) ':right-paren))

;;;;------------------------------------------------------------------------
;;;;Exponent Detection
;;;;------------------------------------------------------------------------

(defun detect-exp (lexed-line)
  "Detects whether there is an exponent."
  (equal (car (second lexed-line)) ':exponent))


;;;;------------------------------------------------------------------------
;;;;Fraction Detection
;;;;------------------------------------------------------------------------

(defun detect-frac (lexed-line)
  "Detects whether there is a fraction."
  (equal (cdr (second lexed-line)) ':/))

;;;;------------------------------------------------------------------------
;;;;Add/sub/mult/equal-chain Detection
;;;;------------------------------------------------------------------------

(defun detect-asm-chain (lexed-line)
  "Detects whether the first object is a member of an add/subtract/multiply chain."
  (member (cdr (second lexed-line)) '("=" "+" "-" "*" ) :test 'equal))

(defun detect-asm-chain-end (lexed-line)
  "Detects whether the asm chain should end."
  (and (not (member (cdr (first lexed-line)) '("=" "+" "-" "*") :test 'equal))
       (not (member (car (first lexed-line)) '(:number :variable :block)))))

;;;;------------------------------------------------------------------------
