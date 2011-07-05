;;;; formulador.lisp
;;;; Copyright (c) 2011 Robert Smith

(in-package :formulador)

(defclass box ()
  ()
  (:documentation "Base class for boxes."))

(defgeneric width (box))
(defgeneric height (box))
(defgeneric baseline (box))

;;; Simple boxes.

(defclass simple-box (box)
  ((width :accessor simple.width :initarg :width)
   (height :accessor simple.height :initarg :height)
   (baseline :accessor simple.baseline :initarg :baseline)
   )
  (:documentation "A basic element which has no sub-boxess. For
  example, a single glyph."))

(defmethod width ((box simple-box))
  (simple.width box))

(defmethod height ((box simple-box))
  (simple.height box))

(defmethod baseline ((box simple-box))
  (simple.baseline box))

(defparameter empty-box
  (make-instance 'simple-box :width 0 :height 0 :baseline 0))

;;; Compound boxes.

(defclass compound-box (box)
  (children)
  (:documentation "Base class for boxes which are composed of
  sub-boxes."))

(defgeneric compound-box-p (box))

(defmethod compound-box-p ((box compound-box))
  t)

(defmethod compound-box-p (box)
  nil)

(defclass row-box (compound-box)
  ((children :accessor children))
  (:documentation "A series of boxes aligned in a row."))

(defmethod width ((box row-box))
  "width(rowbox) = SUM_i width(child_i)"
  (reduce #'+ (children box) :key #'width))

(defmethod baseline ((box row-box))
  (loop
     :for child :in (children box)
     :maximize (baseline child)))

(defmethod height ((box row-box))
  (+ (baseline box)
     (loop
        :for child :in (children box)
        :maximize (- (height child) (baseline child)))))

