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
         (+ (tree-size (car list))
	      (tree-size (cdr list))))))

(defun paren-length-backend (lexed-list counter)
  (cond ((null lexed-list) counter)
        ((detect-end-paren lexed-list) counter)
        (t (paren-length-backend (rest lexed-list) (+ counter 1)))))

(defun paren-length (lexed-list)
  (paren-length-backend lexed-list 0))
            
(defun make-parens-backend (lexed-list)
  "Given a list following a left-paren, returns all items contained within the parenthetical."
  (cond ((null lexed-list) nil)
        ((detect-end-paren lexed-list) nil)
	((detect-paren lexed-list)
         (cons (make-parens (rest lexed-list))
               (make-parens-backend (nthcdr (+ 1 (paren-length (rest lexed-list))) lexed-list))))
;	 (cons (cons 'formulador::parens-box (first (make-parens-group (rest lexed-list))))
;	       (make-parens-group
;		(nthcdr (tree-size (make-parens-group (rest lexed-list)))
;			lexed-list))))
	((detect-exp lexed-list)
	 (cons (make-exponent lexed-list) ;append (list 'formulador::padding (length)
	       (make-parens-backend (rest (rest (rest lexed-list))))))
	((detect-frac lexed-list)
	 (cons (frac-group lexed-list)
	       (make-parens-backend (rest (rest (rest lexed-list))))))
	((detect-infix-chain lexed-list)
	 (cons (infix-chain lexed-list)
	       (make-parens-backend (nthcdr (+ 1 (detect-infix-chain-length lexed-list))
                                            lexed-list))))
	((detect-block lexed-list)
	 (cons (car (block-eval (cdar lexed-list)))
	       (make-parens-backend
		(nthcdr (+ 1 (block-length lexed-list)) lexed-list))))
	(t (cons (first lexed-list)
                 (make-parens-backend (rest lexed-list))))))

(defun make-parens (lexed-list)
  (formulador::parens-box (make-parens-backend lexed-list)))

;;;;------------------------------------------------------------------------
