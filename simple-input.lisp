;;;;simple-input.lisp

(in-package #:formulador)

(defconstant operator-conversion-table '(("(/" '(frac-box ))))


(defun simple-parser (string-formula)
  (cond ((zerop (length string-formula)) nil)
	(t (cons (subseq string-formula 0 1)
		 (simple-parser (subseq string-formula 1))))))

;;;;variables for simple-eval
(defvar parser-parcel nil)
(defvar parsed-list '())

(defun parse-groups (parsed-command)
  (cond ((null parsed-command) (reverse parsed-list))
	((equal (first parsed-command) " ")
	 (progn (setq parsed-list (cons parser-parcel parsed-list))
		(setq parser-parcel nil)
		(parse-groups (rest parsed-command))))
	((equal (first parsed-command) ")")
	 (progn (setq parsed-list (cons parser-parcel parsed-list))
		(setq parsed-list (cons ")" parsed-list))
		(setq parser-parcel nil)
		(parse-groups (rest parsed-command))))
	(t (progn (setq parser-parcel
			(concatenate 'string parser-parcel
				     (first parsed-command)))
		  (parse-groups (rest parsed-command))))))

(defun parse-groups-tester (string-command)
  (setq parser-parcel nil)
  (setq parsed-list nil)
  (parse-groups (simple-parser string-command)))


;;;;for (/ => (frac-box
        ;(+ => arg1 + arg2 ... argn)

;;;;read parse-groups, for each element return lisp text
(defun parse-evaluator (parsed-list)
  (cond ((null parsed-list) nil)
	((equal (first parsed-list) "(/")
	 (cons "(frac-box " (parse-evaluator (rest parsed-list))))
	((equal (first parsed-list) ")")
	 (cons ")" (parse-evaluator (rest parsed-list))))
	(t (cons (concatenate 'string
			      "(box " "\"" (first parsed-list) "\" "  ")")
		 (parse-evaluator (rest parsed-list))))))
;;;output a list of lisp commands, then concatenate them

(reduce #'concatenate 'string (parse-evaluator (parse-groups-test)))

(defun list-concatenator (parsed-list)
  (cond ((null parsed-list) nil)
	(t (concatenate 'string (first parsed-list)
			(list-concatenator (rest parsed-list))))))



;;;Prefix to infix
(defvar infixate (operator list-of-))
;;;

(defun construct-test (formula)
  (concatenate 'string "(draw "
	       (list-concatenator
		(parse-evaluator
		 (parse-groups-tester (write-to-string formula)))) ")"))

;(construct-test '(/ r w))
;"(draw (frac-box (box \"R\" )(box \"W\" )))"

;;;;
;;;Find all commands that use prefix notation
;"/" 

;;;what commands use infix notation?
;(+ r w) => (box "r") (box "+") (box "-")
; "+" "-" "/"
