;;;; ncurses.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith
;;;;
;;;; This file contains extensions to CL-CHARMS.

(in-package #:formulador-editor)

(defun window-dimensions (window)
  (let (width height)
    (charms:getmaxyx window height width)
    (values width height)))

(defun cursor-position (window)
  (let (x y)
    (charms:getyx window y x)
    (values x y)))

(defmacro with-restored-cursor (screen &body body)
  (let ((gscreen (gensym "SCREEN-"))
        (cursor-x (gensym "CURSOR-X"))
        (cursor-y (gensym "CURSOR-Y")))
    `(let ((,gscreen ,screen))
       (multiple-value-bind (,cursor-x ,cursor-y)
           (cursor-position ,gscreen)
         (multiple-value-prog1 (progn ,@body)
           (charms:wmove ,gscreen ,cursor-y ,cursor-x))))))

(defun cursor-up (window)
  (multiple-value-bind (x y) (cursor-position window)
    (charms:wmove window (max 0 (1- y)) x)))

(defun cursor-down (window)
  (multiple-value-bind (x y) (cursor-position window)
    (charms:wmove window (1+ y) x)))

(defun cursor-right (window)
  (multiple-value-bind (x y) (cursor-position window)
    (charms:wmove window y (1+ x))))

(defun cursor-left (window)
  (multiple-value-bind (x y) (cursor-position window)
    (charms:wmove window y (max 0 (1- x)))))
