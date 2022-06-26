;;;; formulador-lite/operators.lisp
;;;;
;;;; Copyright (c) 2021 Izaak Walton

(in-package :formulador-lite)

;;; Exponents

(defun make-exponent (lexed-list)
  (eval (list 'formulador::script-box
              (if (detect-block lexed-list)
                  (car (block-eval (cdar lexed-list)))
	          (cdr (first lexed-list)))
              ':superscript
	      (if (detect-block (rest (rest lexed-list)))
	          (car (block-eval (cdaddr lexed-list))) 
	          (cdr (third lexed-list))))))

;;; code to fit into lexer- convert fractional exponents into x/y notation
              ;(cond ((and (detect-block lexed-list)
                 ;         (equal (first (second (second (car (cddr lexed-list)))))
	;		         ':frac))
	       ;(list 'formulador::box
		;     (concatenate 'string
		;		  (cdar (cadr (third lexed-list)))
		;		  "/"
                                        ;		  (cdr (third (cadr (third lexed-list)))))))

;;; Fractions

(defun frac-group (lexed-list)
  "Generates a (frac-box from the first three elements of a lexed list."
  (eval (list 'formulador::frac-box
	      (if (detect-block lexed-list)
                  (car (block-eval (cdar lexed-list)))
	          (cdr (first lexed-list)))
	      (if (detect-block (rest (rest lexed-list)))
	          (car (block-eval (cdaddr lexed-list)))
	          (cdr (third lexed-list))))))

;;; Infix Operator Chains (+ - * =)

(defun infix-chain-backend (lexed-list)
  (cond ((or (detect-infix-chain-end lexed-list)
	     (detect-end-paren lexed-list)
	     (detect-end-brack lexed-list))
	 nil)
	((detect-block lexed-list)
	 (cons (car (block-eval (cdar lexed-list)))
	       (infix-chain-backend (rest lexed-list))))
	(t (cons (cdr (first lexed-list))
		 (infix-chain-backend (rest lexed-list))))))

(defun infix-chain (lexed-list)
  (eval (cons 'formulador::glue (infix-chain-backend lexed-list))))
;;;;------------------------------------------------------------------------
