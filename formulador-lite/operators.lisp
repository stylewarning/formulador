;;;; formulador-lite/operators.lisp
;;;;
;;;; Copyright (c) 2021 Izaak Walton

(in-package :formulador-lite)

;;; Exponents

(defun make-exponent (lexed-list)
  (eval (list 'formulador::script-box
	(if (equal (car (first lexed-list)) ':block)
            (car (block-eval (second (first lexed-list))))
	    (cdr (first lexed-list)))
	':superscript
	(cond ((and (equal (car (third lexed-list)) ':block)
		    (equal (first (second (second (car (cddr lexed-list)))))
			   ':frac))
	       (list 'formulador::box
		     (concatenate 'string
				  (cdar (cadr (third lexed-list)))
				  "/"
				  (cdr (third (cadr (third lexed-list)))))))
	      ((equal (car (third lexed-list)) ':block)
	       (car (block-eval (second (third lexed-list))))) 
	      (t (cdr (third lexed-list)))))))

;;; Fractions

(defun frac-group (lexed-list)
  "Generates a (frac-box from the first three elements of a lexed list."
  (eval (list 'formulador::frac-box
	      (if (equal (car (first lexed-list)) ':block)
		  (list 'formulador::row-box
			(list 'list
			      (car (block-eval (second (first lexed-list))))))
	          (cdr (first lexed-list)))
	      (if (equal (car (third lexed-list)) ':block)
	          (list 'formulador::row-box
		        (list 'list
			      (car (block-eval (second (third lexed-list))))))
	          (cdr (third lexed-list))))))

;;; Infix Operator Chains (+ - * =)

(defun infix-chain-backend (lexed-list) ; add detect-parens system?
  (cond ((or (detect-infix-chain-end lexed-list)
	     (detect-end-paren lexed-list)
	     (detect-end-brack lexed-list))
	 nil)
	((detect-block lexed-list)
	 (cons (car (block-eval (cadr (first lexed-list))))
	       (infix-chain-backend (rest lexed-list))))
	(t (cons (cdr (first lexed-list))
		 (infix-chain-backend (rest lexed-list))))))

(defun infix-chain (lexed-list)
  (eval (cons 'formulador::glue (infix-chain-backend lexed-list))))
;;;;------------------------------------------------------------------------
