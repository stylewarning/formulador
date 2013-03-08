;;;; canvas.lisp
;;;; Copyright (c) 2013 Robert Smith

(in-package #:formulador)

(defstruct (canvas (:constructor %make-canvas)
                   (:predicate canvasp)
                   (:print-function (lambda (canvas stream depth)
                                      (declare (ignore depth))
                                      (print-canvas canvas stream))))
  data)

(defun make-canvas (width height)
  (%make-canvas :data (make-array (list height width)
                                  :element-type 'character
                                  :initial-element #\Space)))

(defun canvas-dimensions (canvas)
  (reverse (array-dimensions (canvas-data canvas))))

(defun canvas-ref (canvas x y)
  (aref (canvas-data canvas) y x))

(defun canvas-set (canvas x y new-data)
  (setf (aref (canvas-data canvas) y x)
        new-data))

(defsetf canvas-ref canvas-set)

(defun print-canvas (canvas &optional (stream *standard-output*))
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
                     (terpri stream)))))
