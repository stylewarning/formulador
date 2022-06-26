;;;; formulador-lite/blocks.lisp
;;;;
;;;; Copyright (c) 2021-2022 Izaak Walton

(in-package #:formulador-lite)

;;; Generating a list of blocks and operators

(defun make-block-backend (lexed-list)
  "Builds a block as detected."
  (cond ((null lexed-list) nil)
	((detect-end-brack lexed-list) nil)
	((detect-brack lexed-list)
	 (cons (make-block (rest lexed-list))
	       (make-block-backend (nthcdr (+ 1 (brack-length (rest lexed-list))) (rest lexed-list)))))
	(t (cons (first lexed-list)
		 (make-block-backend (rest lexed-list))))))

(defun make-block (lexed-list)
  "Builds a block from a lexed list."
  (cons ':block (make-block-backend lexed-list)))

(defun tree-size (list)
  "Recursively finds the total length of atoms in a list"
  (cond ((null list) 0)
	((atom list) 1)
	(t (+ (tree-size (car list))
	      (tree-size (cdr list))))))

(defun block-length-backend (block-unit counter)
  (cond ((null block-unit) counter)
        ((detect-end-brack block-unit) counter)
        (t (block-length-backend (rest block-unit) (+ counter 1)))))

(defun block-length (block-unit)
  (block-length-backend block-unit 0))

(defun block-list (lexed-list)
  "Cycles through a lexed list and organizes blocks in order."
  (cond ((null lexed-list) nil)
	((detect-end-brack lexed-list) (block-list (rest lexed-list)))
	((detect-brack lexed-list)
	 (cons (make-block (rest lexed-list))
	       (block-list (nthcdr (+ 1 (brack-length (rest lexed-list))) lexed-list))))
	(t (cons (first lexed-list) (block-list (rest lexed-list))))))

;;;Block Evaluation

(defun block-eval (block-unit)
  "Evaluates a block."
  (cond ((null block-unit) nil)
	((detect-paren (list block-unit))
	 (cons (cons 'formulador::parens-box
		     (make-parens-group (rest block-unit)))
	       (block-eval (nthcdr (+ 1 (tree-size (make-parens-group (rest block-unit))))
			block-unit))))
	((detect-exp block-unit)
	 (cons (make-exponent block-unit)
	       (block-eval (rest (rest (rest block-unit))))))
	((detect-frac block-unit)
	 (cons (frac-group block-unit)
	       (block-eval (rest (rest (rest block-unit))))))
	((detect-infix-chain block-unit)
	 (cons (infix-chain block-unit)
	       (block-eval (nthcdr (+ 1 (detect-infix-chain-length block-unit)) block-unit))))
	((detect-block block-unit)
	 (cons (car (block-eval (cadr (first block-unit))))
	       (block-eval
		(nthcdr (+ 1 (tree-size (block-eval (rest block-unit)))) block-unit))))
        (t (cons (first block-unit) (block-eval (rest block-unit))))))

;;; Cycle through the blockified list and evaluate blocks and operators

(defun block-cycle (blocked-list)
  "Cycles through a blocked list and evaluates the boxes, in addition to operators and variables."   
  (cond ((null blocked-list) nil)
	((detect-paren blocked-list)
	 (cons (cons 'formulador::parens-box (make-parens-group (rest blocked-list)))
	       (block-cycle (nthcdr (tree-size (make-parens-group (rest blocked-list))) blocked-list))))
	((detect-exp blocked-list)
	 (cons (make-exponent blocked-list)
	       (block-cycle (rest (rest (rest blocked-list))))))
	((detect-frac blocked-list)
	 (cons (frac-group blocked-list)
	       (block-cycle (rest (rest (rest blocked-list))))))
	((detect-infix-chain blocked-list)
	 (cons (infix-chain blocked-list)
	       (block-cycle (nthcdr (+ 1 (detect-infix-chain-length blocked-list))
				    blocked-list))))
	((detect-block blocked-list)
	 (cons (car (block-eval (cdar blocked-list)))
	       (block-cycle
		(nthcdr (+ 1 (block-length blocked-list)) blocked-list))))
	(t (cons (first blocked-list)
		 (block-cycle (rest blocked-list))))))

;;;; ------------------------------------------------------------------------	    
