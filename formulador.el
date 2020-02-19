
(require 's)
(require 'pos-tip)

(defun formulador--import()
  (concat
   "(load \""
   (concat (current-buffer-path)
	   "formulador.asd\")\n (require `formulador)"
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
						  ,(formulador--import) ,@commands)
						 )
						)))))
	 ))
     (pos-tip-show command)
     (kill-new command)
  ))

  


(formulador-command

 "
(defun pow (a b)
  (formulador:script-box a :superscript b))

(print (formulador:draw (pow (formulador:box \" | \") (formulador:box \"r\"))))"
 )

