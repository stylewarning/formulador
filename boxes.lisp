;;;; boxes.lisp
;;;;
;;;; Copyright (c) 2013-2019 Robert Smith
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
;;;        v  #                   #     BASELINE (1) (example)
;;;       --- ##################### ---
;;;                                  ^
;;;                                  |
;;;
;;; The height is the total height of the box. This is the total
;;; number of vertical cells needed to paint the object.
;;;
;;; The width is the total width of the box. This is the total number
;;; of horizontal cells needed to paint the object.
;;;
;;; The baseline is a measure of a number of cells from the bottom of
;;; the box. The baseline is how the object "sits" on a line and is
;;; used for alignment. For instance, a fraction is usually aligned at
;;; its vinculum, which would be its baseline.

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

(defgeneric box (object)
  (:documentation "A generic function to box up objects conveniently."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *globally-disable-dimensions-caching* nil
  "Globally disable caching of the dimensions for all objects.")

(defclass box ()
  ((cached-width :initarg :cached-width
                 :accessor box-cached-width
                 :documentation "The cached width of the box.")
   (cached-height :initarg :cached-height
                  :accessor box-cached-height
                  :documentation "The cached height of the box.")
   (cached-baseline :initarg :cached-baseline
                    :accessor box-cached-baseline
                    :documentation "The cached baseline of the box.")
   (dimensions-caching-disabled :initarg :dimensions-caching-disabled
                                :accessor box-dimensions-caching-disabled
                                :documentation "Disable caching of the dimensions for just this object. This may be required if objects which the box contains are mutable."))
  (:default-initargs :cached-width nil
                     :cached-height nil
                     :cached-baseline nil
                     :dimensions-caching-disabled nil)
  (:documentation "Abstract class for a box, which can be rendered as a formula. Boxes are the building blocks of formulas."))

(macrolet ((define-cached-reader (reader accessor)
             `(defmethod ,reader :around ((box box))
                (cond
                  ;; Is the caching disabled anywhere?
                  ((or (box-dimensions-caching-disabled box)
                       *globally-disable-dimensions-caching*)
                   (call-next-method))
                  ;; Caching is enabled, but we haven't computed it
                  ;; before.
                  ((null (,accessor box))
                   (setf (,accessor box) (call-next-method)))
                  ;; Otherwise, return our cached value.
                  (t
                   (,accessor box))))))
  (define-cached-reader width box-cached-width)
  (define-cached-reader height box-cached-height)
  (define-cached-reader baseline box-cached-baseline))


;;; Attempting to box up a box just returns the box.

(defmethod box ((object box))
  object)


;;;;;;;;;;;;;;;;;;;;;;;;;;; The Empty Box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass empty-box (box)
  ()
  (:default-initargs :cached-width 0
                     :cached-height 0
                     :cached-baseline 0)
  (:documentation "An empty box that doesn't render to anything.

This class is intended to be a singleton class, and should be accessed with #'EMPTY-BOX."))

(defun empty-box ()
  "Return an instance of an empty box.

N.B., Successive calls may return the same object."
  (load-time-value (make-instance 'empty-box) t))

(defmethod width ((box empty-box))
  0)

(defmethod height ((box empty-box))
  0)

(defmethod baseline ((box empty-box))
  0)


;;; Allow NIL to be boxed.

(defmethod box ((object null))
  (empty-box))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Glass Box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass glass-box (box)
  ((contents :initarg :contents
             :accessor contents))
  (:default-initargs :dimensions-caching-disabled t)
  (:documentation "A box that simply wraps its contents. An identity box. If G(B) is a glass box wrapping the box B, then G(B) will render the same as B."))

(defun glass-box (contents)
  (make-instance 'glass-box :contents contents))

(defmethod width ((box glass-box))
  (width (contents box)))

(defmethod height ((box glass-box))
  (height (contents box)))

(defmethod baseline ((box glass-box))
  (baseline (contents box)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Frozen Box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass frozen-box (box)
  ((contents :initarg :contents
             :reader contents))
  (:documentation "A box that simply wraps its contents like `GLASS-BOX'. However, it is different from `GLASS-BOX' in that this box exists so that a single conceptual box (composed of several sub-boxes) can be treated as just a single opaque (\"frozen\") box."))

(defun freeze (contents)
  (make-instance 'frozen-box :contents contents))

(defmethod width ((box frozen-box))
  (width (contents box)))

(defmethod height ((box frozen-box))
  (height (contents box)))

(defmethod baseline ((box frozen-box))
  (baseline (contents box)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Phantom Box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass phantom-box (box)
  ((contents :initarg :contents
             :accessor contents))
  (:documentation "A box which will render as empty space, of the same dimensions of its contents."))

(defun phantom (contents)
  (make-instance 'phantom-box :contents contents))

(defmethod width ((box phantom-box))
  (width (contents box)))

(defmethod height ((box phantom-box))
  (height (contents box)))

(defmethod baseline ((box phantom-box))
  (baseline (contents box)))


(defclass hphantom-box (phantom-box)
  ()
  (:documentation "A box which will render as empty space, of zero height and the same width of its contents."))

(defun hphantom (contents)
  (make-instance 'hphantom-box :contents contents))

(defmethod width ((box hphantom-box))
  (width (contents box)))

(defmethod height ((box hphantom-box))
  0)

(defmethod baseline ((box hphantom-box))
  0)


(defclass vphantom-box (phantom-box)
  ()
  (:documentation "A box which will render as empty space, of zero width and the same height and baseline of its contents."))

(defun vphantom (contents)
  (make-instance 'vphantom-box :contents contents))

(defmethod width ((box vphantom-box))
  0)

(defmethod height ((box vphantom-box))
  (height (contents box)))

(defmethod baseline ((box vphantom-box))
  (baseline (contents box)))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Overlap Boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass overlap-box (box)
  ((contents :initarg :contents
             :accessor contents))
  (:documentation "Base class for \"overlap\" boxes. These are boxes to support the LaTeX-esque commands \\llap, \\clap, and \\rlap."))

(defmethod width ((box overlap-box))
  0)

(defmethod height ((box overlap-box))
  0)

(defmethod baseline ((box overlap-box))
  0)


(defclass llap-box (overlap-box)
  ()
  (:documentation "A box which writes its contents left-aligned but has vanishing dimensions."))

(defun llap (contents)
  "Similar to the LaTeX command \\llap."
  (make-instance 'llap-box :contents contents))


(defclass clap-box (overlap-box)
  ()
  (:documentation "A box which writes its contents center-aligned but has vanishing dimensions."))

(defun clap (contents)
  "Similar to the LaTeX command \\clap."
  (make-instance 'clap-box :contents contents))


(defclass rlap-box (overlap-box)
  ()
  (:documentation "A box which writes its contents right-aligned but has vanishing dimensions."))

(defun rlap (contents)
  "Similar to the LaTeX command \\rlap."
  (make-instance 'rlap-box :contents contents))


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


;;; Allow characters and strings to be boxed.

(defmethod box ((object string))
  (string-box object))

(defmethod box ((object character))
  (string-box (string object)))


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
             :accessor contents))
  (:documentation "A box decorated with a rectangular frame. This is often used to highlight a particular portion of a formula."))

(defun frame-box (contents)
  "Construct a `FRAME-BOX' whoe contents are CONTENTS."
  (make-instance 'frame-box :contents contents))

(defmethod width ((box frame-box))
  (+ 2
     (width (contents box))))

(defmethod height ((box frame-box))
  (+ 2
     (height (contents box))))

(defmethod baseline ((box frame-box))
  (1+ (baseline (contents box))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Row Boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype vertical-alignment ()
  "The alignments available for ROW-BOX."
  '(member :top :baseline :middle :bottom))

(defclass row-box (box)
  ((padding :initarg :padding
            :type (integer 0)
            :accessor row-box-padding)
   (contents :initarg :contents
             :accessor row-box-contents)
   (align :initarg :align
          :accessor row-box-align
          :type vertical-alignment
          :documentation "Contents can be aligned :TOP,
          :BASELINE (default), :MIDDLE, or :BOTTOM."))
  (:documentation "A horizontal concatenation of boxes."))

(defun row-box (boxes &key (padding 0) (align ':baseline))
  (make-instance 'row-box :padding padding
                          :align align
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
             :accessor contents))
  (:documentation "Representation of a box which is parenthesized."))

(defun parens-box (contents)
  "Construct a `PARENS-BOX' whose contents are CONTENTS."
  (make-instance 'parens-box :contents contents))

(defmethod width ((box parens-box))
  (let ((w (width (contents box)))
        (h (height (contents box))))
    (+ w
       (if (<= 0 h 2)
           2                            ; Small contents are printed as (x).
           4))))                        ; Large contents have extra spacing.

(defmethod height ((box parens-box))
  (let ((h (height (contents box))))
    (if (zerop h) 1 h)))

(defmethod baseline ((box parens-box))
  (baseline (contents box)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Script Box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass script-box (box)
  ((base :initarg :base
         :accessor script-box-base)
   (superscript :initarg :superscript
                :accessor script-box-superscript)
   (subscript :initarg :subscript
              :accessor script-box-subscript)
   ;; TODO:
   ;; presuperscript
   ;; presubscript

   ;; XXX: Should we add these? I'm not sure they make sense in a
   ;; script box. LIMITS-BOX has this functionality.
   ;;
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
  ((base :initarg :base
         :accessor sqrt-box-base)
   (power :initarg :power
          :accessor sqrt-box-power))
  (:documentation "Representation of an object whose root is being taken."))

(defun sqrt-box (base &key (power (empty-box)))
  "Construct a `SQRT-BOX' around BASE, optionally whose power is POWER."
  (assert (or (zerop (height power))
              (= 1 (height power)))
          (power)
          "The POWER of a SQRT-BOX must have a height of 0 or 1.")
  (make-instance 'sqrt-box :base base
                           :power power))

(defmethod width ((box sqrt-box))
  (+ 2
     (max 0 (1- (width (sqrt-box-power box))))
     (max 1 (width (sqrt-box-base box)))))

(defmethod height ((box sqrt-box))
  (1+ (height (sqrt-box-base box))))

(defmethod baseline ((box sqrt-box))
  (baseline (sqrt-box-base box)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; COLUMN BOXES ;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype horizontal-alignment ()
  "HORIZONTAL-ALIGNMENT specifies the alignments for COLUMN-BOX."
  '(member :left :middle :right))

(defclass column-box (box)
  ((padding :initarg :padding
            :type (integer 0)
            :accessor column-box-padding)
   (align :initarg :align
          :accessor column-box-align
          :type horizontal-alignment
          :documentation "Contents can be aligned :LEFT (default),
          :MIDDLE, or :RIGHT.")
   (contents :initarg :contents
             :accessor column-box-contents))
  (:documentation "A vertical concatenation of boxes."))

(defun column-box (boxes &key (padding 0) (align :left))
  (make-instance 'column-box :padding padding
                             :align align
                             :contents boxes))

(defmethod width ((box column-box))
  (maximum (column-box-contents box)
           :key #'width))

(defmethod height ((box column-box))
  (let* ((items (column-box-contents box))
         (nb-items (length items)))
    (+ (sum items :key #'height)
       (if (zerop nb-items) ; account for padding
           0
           (* (1- nb-items)
              (column-box-padding box))))))

(defmethod baseline ((box column-box))
  (floor (height box) 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; ARRAY BOXES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass array-box (box)
  ((contents :initarg :contents
             :reader array-box-contents)
   (row-spacing :initarg :row-spacing
                :reader array-box-row-spacing
                :documentation "The number of empty cells between rows.")
   (column-padding :initarg :column-spacing
                   :reader array-box-column-spacing
                   :documentation "The number of empty cells between columns.")
   ;; These are "private" members of the class.
   ;;
   ;; The rows are stored for their *height*, and the columns are
   ;; stored for their *width*!
   (rows :accessor %array-box-rows)
   (columns :accessor %array-box-columns))
  (:default-initargs :row-spacing 0
                     :column-spacing 1)
  (:documentation "An array of boxes where each row and column has independent alignment."))

(defun make-array-box (rows cols &key (row-alignment
                                       (make-list rows :initial-element ':baseline))
                                      (column-alignment
                                       (make-list cols :initial-element ':middle)))
  (let ((contents (make-array (list rows cols))))
    (loop :for i :below (array-total-size contents)
          :for a := (box (prin1-to-string (- 10000 (random 20000))))
          :for b := (box (prin1-to-string (- 10000 (random 20000))))
          :for n := (box (prin1-to-string (1+ (random 9))))
          :do (setf (row-major-aref contents i)
                    (alexandria:whichever
                     (box a)
                     (sqrt-box (frac-box (box a) (box b)) :power (box n))
                     (script-box (parens-box (frac-box (box a) (box b))) :superscript (box n)))))
    (let ((box (make-instance 'array-box :contents contents)))
      (setf (%array-box-rows box)
            (loop :for row :below rows
                  :for align :in row-alignment
                  :for row-box := (row-box (loop :for col :below cols
                                                 :collect (aref contents row col))
                                           :align align)
                  :collect row-box))
      (setf (%array-box-columns box)
            (loop :for col :below cols
                  :for align :in column-alignment
                  :for col-box := (column-box (loop :for row :below rows
                                                    :collect (aref contents row col))
                                               :align align)
                  :collect col-box))
      ;; return the box
      box)))

(defmethod width ((box array-box))
  (+ (sum (%array-box-columns box) :key #'width)
     (* (array-box-column-spacing box)
        (1- (array-dimension (array-box-contents box) 1)))))

(defmethod height ((box array-box))
  (+ (sum (%array-box-rows box) :key #'height)
     (* (array-box-row-spacing box)
        (1- (array-dimension (array-box-contents box) 0)))))

(defmethod baseline ((box array-box))
  (floor (height box) 2))

