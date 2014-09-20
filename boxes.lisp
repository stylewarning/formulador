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

(defclass box ()
  ((cached-width :initform nil
                 :initarg :cached-width
                 :accessor box-cached-width
                 :documentation "The cached width of the box.")
   (cached-height :initform nil
                  :initarg :cached-height
                  :accessor box-cached-height
                  :documentation "The cached height of the box.")
   (cached-baseline :initform nil
                    :initarg :cached-baseline
                    :accessor box-cached-baseline
                    :documentation "The cached baseline of the box."))
  (:documentation "Abstract class for a box, which can be rendered as a formula. Boxes are the building blocks of formulas."))

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

(defclass empty-box (box)
  ()
  (:documentation "An empty box that doesn't render to anything.

This class is intended to be a singleton class, and should be accessed with #'EMPTY-BOX."))

(let ((box (make-instance 'empty-box :cached-width 0
                                     :cached-height 0
                                     :cached-baseline 0)))
  (defun empty-box ()
    box))

(defmethod width ((box empty-box))
  0)

(defmethod height ((box empty-box))
  0)

(defmethod baseline ((box empty-box))
  0)

;;; TODO: Should we make NIL an empty box?

#+#:ignore
(progn
  (defmethod width ((box null))
    0)

  (defmethod height ((box null))
    0)

  (defmethod baseline ((box null))
    0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Glass Box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass glass-box (box)
  ((contents :initarg :contents
             :accessor glass-box-contents))
  (:documentation "A box that simply wraps its contents. If G(B) is a glass box wrapping the box B, then G(B) will render the same as B."))

(defun glass-box (contents)
  (make-instance 'glass-box :contents contents))

(defmethod width ((box glass-box))
  (width (glass-box-contents box)))

(defmethod height ((box glass-box))
  (height (glass-box-contents box)))

(defmethod baseline ((box glass-box))
  (baseline (glass-box-contents box)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Phantom Box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass phantom-box (box)
  ((contents :initarg :contents
             :accessor phantom-box-contents))
  (:documentation "A box which will render as empty space, of the same dimensions of its contents."))

(defun phantom-box (contents)
  (make-instance 'phantom-box :contents contents))

(defmethod width ((box phantom-box))
  (width (phantom-box-contents box)))

(defmethod height ((box phantom-box))
  (height (phantom-box-contents box)))

(defmethod baseline ((box phantom-box))
  (baseline (phantom-box-contents box)))

;;;;;;;;;;;;;;;;;;;;;;;; Characters as boxes ;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Characters are conveniently considered types of boxes.

(defmethod width ((char character))
  1)

(defmethod height ((char character))
  1)

(defmethod baseline ((char character))
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;; Strings as boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Strings are conveniently considered types of boxes.

(defmethod width ((str string))
  (length str))

(defmethod height ((str string))
  1)

(defmethod baseline ((str string))
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; String Boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass string-box (box)
  ((string :initarg :string
           :type string
           :accessor string-box-string))
  (:documentation "A box whose content is just a string."))

(defun string-box (string)
  (make-instance 'string-box :string string))

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

(defclass frac-box (box)
  ((numerator :initarg :numerator
              :accessor frac-box-numerator
              :documentation "The numerator of a fraction.")
   (denominator :initarg :denominator
                :accessor frac-box-denominator
                :documentation "The denominator of a fraction."))
  (:documentation "A box representing a (vertically printed) fraction."))

(defun frac-box (numerator denominator)
  (make-instance 'frac-box :numerator numerator
                           :denominator denominator))

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

(defclass frame-box (box)
  ((contents :initarg :contents
             :accessor frame-box-contents))
  (:documentation "A box decorated with a rectangular frame. This is often used to highlight a particular portion of a formula."))

(defun frame-box (contents)
  "Construct a `FRAME-BOX' whoe contents are CONTENTS."
  (make-instance 'frame-box :contents contents))

(defmethod width ((box frame-box))
  (+ 2
     (width (frame-box-contents box))))

(defmethod height ((box frame-box))
  (+ 2
     (height (frame-box-contents box))))

(defmethod baseline ((box frame-box))
  (1+ (baseline (frame-box-contents box))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Row Boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass row-box (box)
  ((padding :initarg :padding
            :type (integer 0)
            :accessor row-box-padding)
   (contents :initarg :contents
             :accessor row-box-contents))
  (:documentation "A horizontal concatenation of boxes."))

(defun row-box (boxes &key (padding 0))
  (make-instance 'row-box :padding padding
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

(defclass picture-box (box)
  ((picture :initarg :picture
            :accessor picture-box-picture)
   (baseline :initarg :baseline
             :accessor picture-box-baseline))
  (:documentation "A box representing a two-dimensional picture (typically composed of text art)."))

(defun picture-box (picture &key (baseline 0))
  "Construct a `PICTURE-BOX' whose picture is PICTURE at the baseline BASELINE."
  (make-instance 'picture-box :picture picture
                              :baseline baseline))

(defmethod width ((box picture-box))
  (maximum (picture-box-picture box)
           :key #'length))

(defmethod height ((box picture-box))
  (length (picture-box-picture box)))

(defmethod baseline ((box picture-box))
  (picture-box-baseline box))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Parens Box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass parens-box (box)
  ((contents :initarg :contents
             :accessor parens-box-contents))
  (:documentation "Representation of a box which is parenthesized."))

(defun parens-box (contents)
  "Construct a `PARENS-BOX' whose contents are CONTENTS."
  (make-instance 'parens-box :contents contents))

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

(defclass script-box (box)
  ((base :initarg :base
         :accessor script-box-base)
   (superscript :initarg :superscript
                :accessor script-box-superscript)
   (subscript :initarg :subscript
              :accessor script-box-subscript)
   ;; presuperscript
   ;; presubscript
   ;; over
   ;; under
   )
  (:documentation "A box which contains superscripts or subscripts."))

(defun script-box (base &key (superscript (empty-box)) 
                             (subscript (empty-box)))
  "Construct a `SCRIPT-BOX' whose base is BASE, optionally with a superscript box SUPERSCRIPT and subscript box SUBSCRIPT."
  (make-instance 'script-box :base base
                             :superscript superscript
                             :subscript subscript))

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

(defclass limits-box (box)
  ((base :initarg :base
         :accessor limits-box-base)
   (above :initarg :above
          :accessor limits-box-above)
   (below :initarg :below
          :accessor limits-box-below))
  (:documentation "A box with \"limit\" decorations above and below it."))

(defun limits-box (base &key (above (empty-box))
                             (below (empty-box)))
  (make-instance 'limits-box :base base
                             :above above
                             :below below))

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


;;;;;;;;;;;;;;;;;;;;;;;;;; Square Root Box ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sqrt-box (box)
  ((contents :initarg :contents
             :accessor sqrt-box-contents)
   (power :initarg :power
          :accessor sqrt-box-power))
  (:documentation "Representation of an object whose root is being taken."))

(defun sqrt-box (contents &key (power (empty-box)))
  "Construct a `SQRT-BOX' of the contents CONTENTS, optionally whose power is POWER."
  (assert (or (zerop (height power))
              (= 1 (height power)))
          (power)
          "The POWER of a SQRT-BOX must have a height of 0 or 1.")
  (make-instance 'sqrt-box :contents contents
                           :power power))

(defmethod width ((box sqrt-box))
  (+ 2
     (max 0 (1- (width (sqrt-box-power box))))
     (max 1 (width (sqrt-box-contents box)))))

(defmethod height ((box sqrt-box))
  (1+ (height (sqrt-box-contents box))))

(defmethod baseline ((box sqrt-box))
  (baseline (sqrt-box-contents box)))


;;;;;;;;;
;;; column-box

