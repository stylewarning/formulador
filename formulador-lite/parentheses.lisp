;;;; formulador-lite/parentheses.lisp
;;;;
;;;; Copyright (c) 2021 Izaak Walton

(in-package #:formulador-lite)


(defun tree-size (list)
  "Recursively finds the total length of atoms in a list"
  (cond ((null list)
         0)
	((atom list)
         1)
	(t
         (+ (deep-length (car list))
	      (deep-length (cdr list))))))

(defun make-parens-group (lexed-list)
  "Given a list following a left-paren, returns all items contained within the parenthetical."
  (cond ((detect-end-paren lexed-list)
         nil)
	((detect-paren lexed-list)
	 (cons (cons 'formulador::parens-box (first (make-parens-group (rest lexed-list))))
	       (make-parens-group
		(nthcdr (deep-length (make-parens-group (rest lexed-list)))
			lexed-list))))
	((detect-exp lexed-list)
	 (cons (make-exponent lexed-list) ;append (list 'formulador::padding (length)
	       (make-parens-group (rest (rest (rest lexed-list))))))
	((detect-frac lexed-list)
	 (cons (frac-group lexed-list)
	       (make-parens-group (rest (rest (rest lexed-list))))))
	((detect-asm-chain lexed-list)
	 (cons (cons 'formulador::glue (asm-chain lexed-list))
	       (make-parens-group
		(nthcdr (+ 1 (length (asm-chain (rest lexed-list))))
			lexed-list))))
	((detect-block lexed-list)
	 (cons (car (block-eval (cadr (first lexed-list))))
	       (make-parens-group
		(nthcdr (deep-length (block-eval (rest lexed-list))) lexed-list))))
	(t
         (cons (first lexed-list)
		 (make-parens-group (rest lexed-list))))))

;;;;------------------------------------------------------------------------
