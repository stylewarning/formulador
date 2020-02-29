;;; formulador.el --- Allow usage of formulador from within emacs   -*- lexical-binding: t -*-

;; Copyright (C) 2019-  Andres Mariscal

;; Author: Andres Mariscal <carlos.mariscal.melgar@gmail.com>

;; URL: https://github.com/serialdev/tiqsi-emacs
;; Keywords: lisp
;; Version: 0
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

(require 's)


(defun current-buffer-path()
  (file-name-directory (buffer-file-name)))


(defun formulador--import()
  (concat
   "(load \""
   (concat (current-buffer-path)
	   "formulador.asd\")\n (require `formulador)


(defun pow (a b)
  (formulador:script-box (formulador:box a) :superscript (formulador:box b )))



(defun sub (a b)
  (formulador:script-box (formulador:box a) :subscript (formulador:box b)))

(defun frak (a b)
  (formulador:frac-box (formulador:box a)
		       (formulador:box b)))

(defun rowb (a b)
  (formulador:row-box (list (formulador:box a) (formulador:box b))))

(print
(formulador:draw 
"
	   )
   ))

(defun formulador/scratch ()
  (interactive)
  (let ((buf (get-buffer-create "*formulador-scratch*" ))
	(data (formulador--import)))
    (with-current-buffer buf
      (pop-to-buffer buf)
      (common-lisp-mode)
      (insert data)
      )))

(defmacro formulador--ros-commands (&rest commands)
  `(shell-command-to-string
   (concat
    (concat "ros --eval '" ,@commands)
    "'")))


(defmacro formulador-command (&rest commands)
  `(let ((command
	  (car(s-split "with"
		       (car
			(cdr
			 (s-split ":CANVAS"
				  (formulador--ros-commands
				   (concat
				    ,(formulador--import)
				    (concat ,@commands " ))"))
				   ))))))))
     (message command)
     (kill-new command)
  ))



; ------------------------------------------------------------------------- ;
;                               Usage Example                               ;
; ------------------------------------------------------------------------- ;

;; (formulador-command "(frak \"2\" \"efr\")")


;; (formulador-command 
;; "
;; (rowb (frak \"1\" \"2\") (formulador:limits-box formulador:+sigma+ :below (formulador:box \"(i,j)∈E)\"))
;; "
;; )

;; (formulador-command "(rowb
;;  (frak \"1\" \"2\") (formulador:box\"(i,j)∈E)\" )
;; )")


;; (formulador-command
;; "
;;  (rowb
;;   (rowb \"φ(Y,λ) = \"
;; 	(frak \"1\" \"2\")  )


;;   (rowb
;;   (rowb
;;    (rowb 
;;    (formulador:limits-box formulador:+sigma+  :below (formulador:box \"(i,j)∈E\")  )


;;    (rowb (rowb 
;;    (rowb

;;     (rowb (rowb \"(\" (sub (formulador:box \"W\") (formulador:box \"ij \")) )
;; 	  (rowb \"-< \"  (sub \"Y\" \"i,\" )))
;;     (rowb
;;      (sub \"Y\" \"j\" )
;;      (pow \">(\" \"2\" )))
;;    (rowb \"+\" (formulador:frac-box (formulador:box \"λ\") (formulador:box \"2\")  ) ))

;;      (formulador:limits-box formulador:+sigma+  :below (formulador:box \"i\")  ))


;;    )
;;    (rowb \"||\" (sub \"Y\" \"i\")))
;;   (rowb \"|\" (rowb (pow \"|\" \"2\") \",\")))

;;   )

;; "
;;  )



;; +---------------+
;; |     ===       |
;; | 1   \         |
;; |---   >   (W   |
;; | 2   /      ij |
;; |     ===       |
;; |   (i,j)∈E     |
;; +---------------+


(provide 'formulador)

;;; formulador.el ends here
