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
(require 'pos-tip)

(defun formulador--import()
  (concat
   "(load \""
   (concat (current-buffer-path)
	   "formulador.asd\")\n (require `formulador)


(defun pow (a b)
  (formulador:script-box a :superscript b))



(defun sub (a b)
  (formulador:script-box a :subscript b))

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


(defmacro formulador--ros-commands (&rest commands)
  `(shell-command-to-string
   (concat
    (concat "ros --eval '" ,@commands)
    "'")))


(defmacro formulador-command (&rest commands)
  `(let ((command
	  (car(s-split "with" (car(cdr (s-split ":CANVAS"
						(formulador--ros-commands
						 (concat
						  ,(formulador--import)
						  (concat ,@commands " ))"))
						 )
						)))))
	 ))
     (pos-tip-show command)
     (kill-new command)
  ))



; ------------------------------------------------------------------------- ;
;                               Usage Example                               ;
; ------------------------------------------------------------------------- ;

;; (formulador-command "(frak \"2\" \"efr\")")



(provide 'formulador)

;;; formulador.el ends here
