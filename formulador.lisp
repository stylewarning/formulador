;;;; formulador.lisp
;;;; Copyright (c) 2011-2012 Robert Smith

(in-package #:formulador)

(defclass box ()
  ()
  (:documentation "Base class for boxes."))

(defgeneric width (box)
  (:documentation "Compute the width of a box BOX."))

(defgeneric height (box)
  (:documentation "Compute the height of a box BOX."))

(defgeneric baseline (box)
  (:documentation "Compute the baseline of a box BOX."))

;;; Simple boxes.

(defclass simple-box (box)
  ((width :accessor width :initarg :width)
   (height :accessor height :initarg :height)
   (baseline :accessor baseline :initarg :baseline))
  (:documentation "A basic element which has no sub-boxes. For
  example, a single glyph."))

(defun simple-box (w h b)
  (make-instance 'simple-box :width w :height h :baseline b))

(with-compilation-unit ()
  (defconstant empty-box (simple-box 0 0 0)))

;;; Compound boxes.

(defclass compound-box (box)
  ((children :accessor children :initarg :children))
  (:documentation "Base class for boxes which are composed of
  sub-boxes."))

;;; Row boxes.

(defclass row-box (compound-box)
  ()
  (:documentation "A series of boxes aligned in a row."))

(defun row-box (&rest boxes)
  (make-instance 'row-box :children boxes))

(defmethod width ((box row-box))
  ;; width(rowbox) = SUM_i width(child_i)
  (reduce #'+ (children box) :key #'width))

(defmethod baseline ((box row-box))
  (loop :for child :in (children box)
        :maximize (baseline child)))

(defmethod height ((box row-box))
  (+ (baseline box)
     (loop :for child :in (children box)
           :maximize (- (height child) (baseline child)))))

;;; Column boxes.

(defclass column-box (compound-box)
  ()
  (:documentation "A series of boxes aligned in a column."))

(defun column-box (&rest boxes)
  (make-instance 'column-box :children boxes))

(defmethod width ((box column-box))
  ;; width(columnbox) = max_i width(child_i)
  (loop :for child :in (children box)
        :maximize (width child)))

(defmethod baseline ((box column-box))
  (/ (height box) 2))

;;; XXX complete
(defmethod height ((box column-box))
  (loop :for child :in (children box)
        :summing (height child)))

;;; Fraction boxes.

;;; Superscript boxes.

;;; Subscript boxes.

;;; General script boxes.

;;; Big operator boxes.

;;; Root boxes.

;;; Over boxes.

;;; Under boxes.

;;; Fenced boxes.

;;; Grid boxes.