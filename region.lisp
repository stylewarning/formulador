;;;; region.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith
;;;;
;;;; Definition of a "region" used to identify which parts of a canvas
;;;; refer to which parts of tree of boxes.

(in-package #:formulador)

(defstruct (region (:predicate regionp)
                   (:constructor make-region (min-x min-y max-x max-y)))
  min-x                                 ; Inclusive
  min-y                                 ; Inclusive
  max-x                                 ; Exclusive
  max-y)                                ; Exclusive

(defun make-region-by-dimensions (x y width height)
  "Construct a new region starting at (X, Y) and having width WIDTH and height HEIGHT."
  (make-region x
               y
               (+ x width)
               (+ y height)))

(defun region= (r1 r2)
  "Are the regions R1 and R2 equivalent?"
  (and (= (region-min-x r1)
          (region-min-x r2))
       (= (region-min-y r1)
          (region-min-y r2))
       (= (region-max-x r1)
          (region-max-x r2))
       (= (region-max-y r1)
          (region-max-y r2))))

(defun in-region-p (r x y)
  "Is the point (X, Y) in the region R?"
  (flet ((in (a x b)
           (and (<= a x)
                (< x b))))
    (and (in (region-min-x r)
             x
             (region-max-x r))
         (in (region-min-y r)
             y
             (region-max-y r)))))

(defun region-area (r)
  "Compute the area of a region R."
  (* (- (1- (region-max-x r))
        (region-min-x r))
     (- (1- (region-max-y r))
        (region-min-y r))))
