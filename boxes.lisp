;;;; boxes.lisp
;;;;
;;;; Copyright (c) 2013-2014 Robert Smith
;;;;
;;;; Basic building blocks of a "formula".

(in-package #:formulador)


;;;;;;;;;;;;;;;;;;;;;;; Generic Box Operations ;;;;;;;;;;;;;;;;;;;;;;;

;;; Here, the # marks represent the "bounding box".
;;;
;;;                  WIDTH (19)
;;;           |<----------------->|
;;;       --- #####################
;;;        ^  #                   #  |
;;; HEIGHT |  #                   #  v
;;;  (4)   |  #-------------------# ---
;;;        v  #                   #     BASELINE (1)
;;;       --- ##################### ---
;;;                                  ^
;;;                                  |

(defgeneric width (object)
  (:documentation "The width of a box. This is the number of characters a box requires horizontally."))

(defgeneric height (object)
  (:documentation "The height of a box. This is the number of characters a box requires vertically."))

(defgeneric baseline (object)
  (:documentation "The baseline of a box. This is the number of characters that a box would need to be translated downward in order to be properly aligned."))

(defun height-above-baseline (object)
  "Compute the height of an object above the baseline."
  (- (height object)
     (baseline object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (box (:constructor %make-box))
  cached-width
  cached-height
  cached-baseline)

(defmethod width :around ((box box))
  (or (box-cached-width box)
      (setf (box-cached-width box)
            (call-next-method box))))

(defmethod height :around ((box box))
  (or (box-cached-height box)
      (setf (box-cached-height box)
            (call-next-method box))))

(defmethod baseline :around ((box box))
  (or (box-cached-baseline box)
      (setf (box-cached-baseline box)
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

(defmethod baseline ((box empty-box))
  0)

;;; Should we make NIL an empty box?

#+#:ignore
(progn
  (defmethod width ((box null))
    0)

  (defmethod height ((box null))
    0)

  (defmethod baseline ((box null))
    0))


;;;;;;;;;;;;;;;;;;;;;;;; Characters as boxes ;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod width ((char character))
  1)

(defmethod height ((char character))
  1)

(defmethod baseline ((char character))
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;; Strings as boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod width ((str string))
  (length str))

(defmethod height ((str string))
  1)

(defmethod baseline ((str string))
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; String Boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (string-box (:include box)
                       (:constructor string-box (string)))
  string)

(defmethod width ((box string-box))
  (length (string-box-string box)))

(defmethod height ((box string-box))
  (declare (ignore box))
  1)

(defmethod baseline ((box string-box))
  (declare (ignore box))
  0)


;;;;;;;;;;;;;;;;;;;;;;;;;;; Fraction Boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *frac-box-vinculum-padding* 1
  "The amount by which to stretch each side of the vinculum.")

(defstruct (frac-box (:include box)
                     (:constructor frac-box (numerator denominator)))
  numerator
  denominator)

(defmethod width ((box frac-box))
  (+ (* 2 *frac-box-vinculum-padding*)
     (max (width (frac-box-numerator box))
          (width (frac-box-denominator box)))))

(defmethod height ((box frac-box))
  (+ 1
     (height (frac-box-numerator box))
     (height (frac-box-denominator box))))

(defmethod baseline ((box frac-box))
  (height (frac-box-denominator box)))

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

(defmethod baseline ((box frame-box))
  (1+ (baseline (frame-box-contents box))))

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
    (+ (sum items :key #'width)
       (if (zerop nb-items)
           0
           (* (1- nb-items)
              (row-box-padding box))))))

(defmethod height ((box row-box))
  (+ (baseline box)
     (maximum (row-box-contents box)
              :key #'height-above-baseline)))

(defmethod baseline ((box row-box))
  (maximum (row-box-contents box)
           :key #'baseline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Picture Box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (picture-box (:include box)
                        (:constructor picture-box (picture &key baseline)))
  picture
  (baseline 0))

(defmethod width ((box picture-box))
  (maximum (picture-box-picture box)
           :key #'length))

(defmethod height ((box picture-box))
  (length (picture-box-picture box)))

(defmethod baseline ((box picture-box))
  (picture-box-baseline box))

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

(defmethod baseline ((box parens-box))
  (baseline (parens-box-contents box)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Script Box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (script-box (:include box)
                       (:constructor script-box (base &key superscript subscript)))
  base
  (superscript (empty-box))
  (subscript (empty-box))
  ;; presuperscript
  ;; presubscript
  ;; over
  ;; under
  )

(defmethod width ((box script-box))
  (+ (width (script-box-base box))
     (max (width (script-box-superscript box))
          (width (script-box-subscript box)))))

(defmethod height ((box script-box))
  (+ (height (script-box-base box))
     (height (script-box-superscript box))
     (height (script-box-subscript box))))

(defmethod baseline ((box script-box))
  (+ (baseline (script-box-base box))
     (height (script-box-subscript box))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Limits Box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (limits-box (:include box)
                       (:constructor limits-box (base &key above below)))
  base
  (above (empty-box))
  (below (empty-box)))

(defmethod width ((box limits-box))
  (max (width (limits-box-base box))
       (width (limits-box-above box))
       (width (limits-box-below box))))

(defmethod height ((box limits-box))
  (+ (height (limits-box-base box))
     (height (limits-box-above box))
     (height (limits-box-below box))))

(defmethod baseline ((box limits-box))
  (+ (baseline (limits-box-base box))
     (height (limits-box-below box))))



;;;;;;;;;
;;; column-box


