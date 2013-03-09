;;;; boxes.lisp
;;;; Copyright (c) 2013 Robert Smith

(in-package #:formulador)


;;;;;;;;;;;;;;;;;;;;;;; Generic Box Operations ;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric width (object))
(defgeneric height (object))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (box (:constructor %make-box))
  cached-width
  cached-height)

(defmethod width :around ((box box))
  (or (box-cached-width box)
      (setf (box-cached-width box)
            (call-next-method box))))

(defmethod height :around ((box box))
  (or (box-cached-height box)
      (setf (box-cached-height box)
            (call-next-method box))))


;;;;;;;;;;;;;;;;;;;;;;;;;;; The Empty Box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (empty-box (:include box)
                      (:constructor %empty-box)))

(let ((box (%empty-box :cached-width 0
                       :cached-height 0)))
  (defun empty-box ()
    box))

(defmethod width ((box empty-box))
  0)

(defmethod height ((box empty-box))
  0)

;;;;;;;;;;;;;;;;;;;;;;;; Characters as boxes ;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod width ((char character))
  1)

(defmethod height ((char character))
  1)


;;;;;;;;;;;;;;;;;;;;;;;;;; Strings as boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod width ((str string))
  (length str))

(defmethod height ((str string))
  1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;; String Boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (string-box (:include box)
                       (:constructor string-box (string)))
  string)

(defmethod width ((box string-box))
  (length (string-box-string box)))

(defmethod height ((box string-box))
  (declare (ignore box))
  1)


;;;;;;;;;;;;;;;;;;;;;;;;;;; Fraction Boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *frac-box-vinculum-padding* 1)

(defstruct (frac-box (:include box)
                     (:constructor frac-box (numerator denominator)))
  numerator denominator)

(defmethod width ((box frac-box))
  (+ (* 2 *frac-box-vinculum-padding*)
     (max (width (frac-box-numerator box))
          (width (frac-box-denominator box)))))

(defmethod height ((box frac-box))
  (+ 1
     (height (frac-box-numerator box))
     (height (frac-box-denominator box))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Frame Boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (frame-box (:include box)
                      (:constructor frame-box (contents)))
  contents)

(defmethod width ((box frame-box))
  (+ 2
     (width (frame-box-contents box))))

(defmethod height ((box frame-box))
  (+ 2
     (height (frame-box-contents box))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Row Boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (row-box (:include box)
                    (:constructor %row-box))
  (padding 0 :type (integer 0))
  contents)

(defun row-box (boxes &key (padding 0))
  (%row-box :padding padding
            :contents boxes))

(defmethod width ((box row-box))
  ;; include padding?
  (let* ((items (row-box-contents box))
         (nb-items (length items)))
    (+ (reduce #'+ items :key #'width)
       (if (zerop nb-items)
           0
           (* (1- nb-items)
              (row-box-padding box))))))

(defmethod height ((box row-box))
  (loop :for item :in (row-box-contents box)
        :maximize (height item)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Picture Box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (picture-box (:include box)
                        (:constructor picture-box (picture)))
  picture)

(defmethod width ((box picture-box))
  (reduce #'max (picture-box-picture box) :key #'length
                                          :initial-value 0))

(defmethod height ((box picture-box))
  (length (picture-box-picture box)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Parens Box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (parens-box (:include box)
                       (:constructor parens-box (contents)))
  contents)

(defmethod width ((box parens-box))
  (let ((w (width (parens-box-contents box)))
        (h (height (parens-box-contents box))))
    (+ w
       (if (<= 0 h 2)
           2                            ; Small contents are printed as (x).
           4))))                        ; Large contents have extra spacing.

(defmethod height ((box parens-box))
  (let ((h (height (parens-box-contents box))))
    (if (zerop h) 1 h)))


;;;;;;;;;
;;; column-box
;;; script-box


