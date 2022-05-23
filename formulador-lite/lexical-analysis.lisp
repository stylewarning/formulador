;;;; formulador-lite/lexical-analysis.lisp
;;;;
;;;; Copyright (c) 2021 Izaak Walton

(in-package :formulador-lite)

;;;Defining tokens

(deftype token ()
  '(cons keyword t))

(defun tok (type &optional val)
  (cons type val))

;;;The Analysis

(alexa:define-string-lexer formulexer
  "A lexical analyzer for formulador-lite"
  ((:oper "[=+*-]")
   (:num "\\d+")
   (:symb "[A-Za-z][A-Za-z0-9_]*"))
   ("{{OPER}}" (return (tok #'formulador::box (princ-to-string $@))))
   ("{{NUM}}"  (return (tok #'formulador::box (princ-to-string $@))))
   ("{{SYMB}}" (return (tok #'formulador::box  (princ-to-string $@))))
   ("\\/"      (return (tok :frac (intern $@ 'keyword))))
   ("\\^"      (return (tok :exponent)))
   ("\\("      (return (tok :left-paren)))
   ("\\)"      (return (tok :right-paren)))
   ("\\["      (return (tok :left-brack)))
   ("\\]"      (return (tok :right-brack)))
   ("\\s+"     nil))
  
;(alexa:define-string-lexer formulexer
 ; "A lexical analyzer for formulador input."
;  ((:oper   "[=+*-]")
;   (:num  "\\d+")
;   (:symb "[A-Za-z][A-Za-z0-9_]*"))
 ; ("{{OPER}}" (return (tok :operator (princ-to-string $@))))
 ; ("{{NUM}}"  (return (tok :number (princ-to-string $@))))
;  ("{{SYMB}}" (return (tok :variable (princ-to-string $@))))
;  ("\\/"      (return (tok :frac (intern $@ 'keyword))))
;  ("\\^"      (return (tok :exponent)))
;  ("\\("      (return (tok :left-paren)))
;  ("\\)"      (return (tok :right-paren)))
;  ("\\["      (return (tok :left-brack)))
;  ("\\]"      (return (tok :right-brack)))
;  ("\\s+"     nil))

(defun lex-line (string)
  "Breaks down a formula string into tokens."
  (loop :with lexer := (formulexer string)
	:for tok := (funcall lexer)
	:while tok
	:collect tok))
