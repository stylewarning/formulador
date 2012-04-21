;;;; render.lisp
;;;; Copyright (c) 2011-2012 Robert Smith

;;; Facility to render boxes.

(in-package #:formulador)

(defstruct rectangle
  x-offset
  y-offset
  width
  height
  baseline)

(defun bounding-rectangle (box &optional (x-offset 0) (y-offset 0))
  (make-rectangle :x-offset x-offset
                  :y-offset y-offset
                  :width (width box)
                  :height (height box)
                  :baseline (baseline box)))

(defgeneric make-rectangles (box &key x y))

(defmethod make-rectangles ((box simple-box) &key (x 0) (y 0))
  (list (bounding-rectangle box x y)))

(defmethod make-rectangles ((box row-box) &key (x 0) (y 0))
  (append nil ;(list (bounding-rectangle box x y))
          (let ((x-offset x))
            (loop
               :for child :in (children box)
               :for y-offset := (- (baseline box) (baseline child))
               :appending (make-rectangles child :x x-offset
                                                 :y y-offset)
               :do (incf x-offset (width child))))))

(defmethod make-rectangles ((box column-box) &key (x 0) (y 0))
  (append nil ;(list (bounding-rectangle box x y))
          (let ((y-offset y))
            (loop
               :for child :in (children box)
               :appending (make-rectangles child :x x
                                                 :y y-offset)
               :do (incf y-offset (height child))))))

#+#:ignore
(defparameter *test* (row-box (simple-box 100 100 10)
                              (column-box
                               (simple-box 50 75 25)
                               (simple-box 25 15 5))
                              (simple-box 50 90 30)))

(defun render-rectangles (rectangles &key
                          (output "out.png")
                          (width 1000)
                          (height 1000))
  (vecto:with-canvas (:width width :height height)
    (vecto:set-rgba-stroke 0.0 0.0 0.0 0.5)
    (vecto:set-rgb-fill 1.0 1.0 1.0)
    (vecto:rectangle 0 0 width height)
    (vecto:fill-and-stroke)
    (loop
       :for r :in rectangles
       :do (progn
             (vecto:rectangle (rectangle-x-offset r)
                              (rectangle-y-offset r)
                              (rectangle-width r)
                              (rectangle-height r))
             (vecto:stroke)
             (vecto:with-graphics-state
               (vecto:set-dash-pattern #(10 10 10) 0)
               (vecto:move-to (rectangle-x-offset r)
                              (+ (rectangle-y-offset r)
                                 (rectangle-baseline r)))
               (vecto:line-to (+ (rectangle-x-offset r) (rectangle-width r))
                              (+ (rectangle-y-offset r)
                                 (rectangle-baseline r)))
               (vecto:stroke))))
    (vecto:save-png output)))