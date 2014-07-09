;;;; charmap.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith
;;;;
;;;; This file defines "charmaps", which are used to swap in and out
;;;; different drawing styles.

(in-package #:formulador)

;;; First, some characters.

(defconstant +box-drawings-light-horizontal+     (code-char #x2500))
(defconstant +box-drawings-light-vertical+       (code-char #x2502))
(defconstant +box-drawings-light-down-and-right+ (code-char #x250C))
(defconstant +box-drawings-light-down-and-left+  (code-char #x2510))
(defconstant +box-drawings-light-up-and-right+   (code-char #x2514))
(defconstant +box-drawings-light-up-and-left+    (code-char #x2518))

;;; Vinculum Charmaps

(defvar *ascii-vinculum-charmap* #\-)

(defvar *unicode-vinculum-charmap* +box-drawings-light-horizontal+)

;;; Paren Charmaps

(defstruct paren-charmap
  small-open
  small-close
  
  top-open
  middle-open
  bottom-open
  
  top-close
  middle-close
  bottom-close)

(defvar *ascii-paren-charmap*
  (make-paren-charmap
   :small-open #\(
   :small-close #\)
   
   :top-open #\/
   :middle-open #\|
   :bottom-open #\\
   
   :top-close #\\
   :middle-close #\|
   :bottom-close #\/))

(defvar *unicode-paren-charmap*
  (make-paren-charmap
   :small-open #\(
   :small-close #\)
   
   :top-open (code-char #x239B)
   :middle-open (code-char #x239C)
   :bottom-open (code-char #x239D)
   
   :top-close (code-char #x239E)
   :middle-close (code-char #x239F)
   :bottom-close (code-char #x23A0)))

(defvar *unicode-bracket-charmap*
  (make-paren-charmap
   :small-open #\[
   :small-close #\]
   
   :top-open (code-char #x23A1)
   :middle-open (code-char #x23A2)
   :bottom-open (code-char #x23A3)
   
   :top-close (code-char #x23A4)
   :middle-close (code-char #x23A5)
   :bottom-close (code-char #x23A6)))

(defstruct frame-charmap
  top-left-corner
  top-right-corner
  bottom-left-corner
  bottom-right-corner
  
  left-edge
  right-edge
  top-edge
  bottom-edge)

(defvar *ascii-plain-frame-charmap*
  (make-frame-charmap
   :top-left-corner #\+
   :top-right-corner #\+
   :bottom-left-corner #\+
   :bottom-right-corner #\+
  
   :left-edge #\|
   :right-edge #\|
   :top-edge #\-
   :bottom-edge #\-))

(defvar *macsyma-frame-charmap*
  (make-frame-charmap
   :top-left-corner #\"
   :top-right-corner #\"
   :bottom-left-corner #\"
   :bottom-right-corner #\"
  
   :left-edge #\"
   :right-edge #\"
   :top-edge #\"
   :bottom-edge #\"))
   
(defvar *unicode-plain-frame-charmap*
  (make-frame-charmap
   :top-left-corner +box-drawings-light-down-and-right+
   :top-right-corner +box-drawings-light-down-and-left+
   :bottom-left-corner +box-drawings-light-up-and-right+
   :bottom-right-corner +box-drawings-light-up-and-left+
  
   :left-edge +box-drawings-light-vertical+
   :right-edge +box-drawings-light-vertical+
   :top-edge +box-drawings-light-horizontal+
   :bottom-edge +box-drawings-light-horizontal+))
