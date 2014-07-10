;;;; canvas.lisp
;;;;
;;;; Copyright (c) 2013 Robert Smith
;;;;
;;;; A simple notion of a "canvas" on which we can draw formulas.

(in-package #:formulador)

(defstruct (canvas (:constructor %make-canvas)
                   (:predicate canvasp)
                   (:print-function (lambda (canvas stream depth)
                                      (declare (ignore depth))
                                      (print-canvas canvas stream))))
  data
  region-associations)

(defun make-canvas (width height)
  "Make a new canvas of width WIDTH and height HEIGHT."
  (%make-canvas :data (make-array (list height width)
                                  :element-type 'character
                                  :initial-element #\Space)))

(defun canvas-dimensions (canvas)
  "Return the dimensions of the canvas (WIDTH HEIGHT)."
  (reverse (array-dimensions (canvas-data canvas))))

(defun canvas-ref (canvas x y)
  "Obtain the character (X, Y) in the canvas CANVAS."
  (aref (canvas-data canvas) y x))

(defun canvas-set (canvas x y new-data)
  "Set the character at (X, Y) in the canvas CANVAS to the value NEW-DATA."
  (setf (aref (canvas-data canvas) y x)
        new-data))

(defsetf canvas-ref canvas-set)

(defun add-association (canvas region &optional object)
  "Add the region REGION to the canvas CANVAS, associating it with the object OBJECT."
  (push (cons region object)
        (canvas-region-associations canvas)))

(defun find-associations (canvas x y)
  "Find the regions which contain the point (X, Y) along with their associated objects."
  (loop :for ra :in (canvas-region-associations canvas)
        :when (in-region-p (car ra) x y)
          :collect ra))

(defun objects-at-point (canvas x y)
  "Compute all of the objects at the point (X, Y) in the canvas CANVAS."
  (mapcar #'cdr (find-associations canvas x y)))

(defun print-canvas (canvas &optional (stream *standard-output*))
  (print-unreadable-object (canvas stream :type t)
    (terpri stream)
    (destructuring-bind (width height)
        (canvas-dimensions canvas)
      (loop :initially (write-char #\+ stream) 
            :repeat width
            :do (write-char #\- stream)
            :finally (progn
                       (write-char #\+ stream)
                       (terpri stream)))
      
      (dotimes (y height)
        (write-char #\| stream)
        (dotimes (x width)
          (write-char (canvas-ref canvas x y) stream))
        (write-char #\| stream)
        (terpri stream))
      
      (loop :initially (write-char #\+ stream) 
            :repeat width
            :do (write-char #\- stream)
            :finally (progn
                       (write-char #\+ stream)
                       (terpri stream))))
    (format stream "with ~D defined region~:p"
            (length (canvas-region-associations canvas)))))
