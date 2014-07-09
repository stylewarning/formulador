;;;; utilities.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith
;;;;
;;;; This file contains various utilities.

(in-package #:formulador)

(defun minimum (items &key (key 'identity)
                           (otherwise 0))
  "Comptue the minimum value of ITEMS, optionally applying the unary function KEY to each item. If there are no items, return OTHERWISE, which is 0 by default."
  (if (null items)
      otherwise
      (loop :for item :in items
            :minimize (funcall key item))))

(defun maximum (items &key (key 'identity)
                           (otherwise 0))
  "Comptue the maximum value of ITEMS, optionally applying the unary function KEY to each item. If there are no items, return OTHERWISE, which is 0 by default."
  (if (null items)
      otherwise
      (loop :for item :in items
            :maximize (funcall key item))))

(defun sum (items &key (key 'identity))
  "Compute the sum of ITEMS, optionally applying the unary function KEY to each item."
  (loop :for item :in items
        :sum (funcall key item)))
