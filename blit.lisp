;;;; blit.lisp
;;;;
;;;; Copyright (c) 2013-2014 Robert Smith
;;;;
;;;; The logic to "blit", or write out, the graphical construction of
;;;; a formula.

(in-package #:formulador)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun paint-vertical-line (canvas x y-start y-end &key (char #\|))
  (loop :for y :from y-start :to y-end
        :do (setf (canvas-ref canvas x y)
                  char)))

(defun paint-horizontal-line (canvas y x-start x-end &key (char #\-))
  (loop :for x :from x-start :to x-end
        :do (setf (canvas-ref canvas x y)
                  char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Blitting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric blit (canvas object x y))

(defmethod blit (canvas (box empty-box) x y)
  ;; do nothing
  )

(defmethod blit (canvas (char character) x y)
  (setf (canvas-ref canvas x y) char))

(defmethod blit (canvas (str string) x y)
  (loop :for c :across str
        :for i :from 0
        :do (setf (canvas-ref canvas (+ x i) y)
                  c)))

(defmethod blit (canvas (box string-box) x y)
  (loop :for c :across (string-box-string box)
        :for i :from 0
        :do (setf (canvas-ref canvas (+ x i) y)
                  c)))

(defparameter *vinculum-charmap* *ascii-vinculum-charmap*
  "The charmap to use when drawing the vinculum.")

(defmethod blit (canvas (box frac-box) x y)
  (let* ((total-width (width box))
         (padding (* 2 *frac-box-vinculum-padding*))
         (num-space (floor (- total-width
                              padding
                              (width (frac-box-numerator box))) 2))
         (den-space (floor (- total-width
                              padding
                              (width (frac-box-denominator box))) 2)))
    (let* ((num-x (+ *frac-box-vinculum-padding*
                     num-space
                     x))
           (num-y y)
           (mid-x x)
           (mid-y (+ y (height (frac-box-numerator box))))
           (den-x (+ *frac-box-vinculum-padding*
                     den-space
                     x))
           (den-y (1+ mid-y)))
      ;; Blit the numerator and denominator
      (blit canvas (frac-box-numerator box) num-x num-y)
      (blit canvas (frac-box-denominator box) den-x den-y)
      
      ;; Blit the fraction bar
      (loop :for i :from mid-x :below (+ mid-x total-width)
            :do (setf (canvas-ref canvas i mid-y)
                      *vinculum-charmap*)))))

(defparameter *frame-charmap* *ascii-plain-frame-charmap*
  "The charmap to use when drawing a frame.")

(defmethod blit (canvas (box frame-box) x y)  
  ;; Blit the left side.
  (paint-vertical-line canvas
                       x
                       (1+ y)
                       (+ y (height (frame-box-contents box)))
                       :char (frame-charmap-left-edge *frame-charmap*))
  
  ;; Blit the right side.
  (paint-vertical-line canvas 
                       (+ 1 x (width (frame-box-contents box)))
                       (1+ y)
                       (+ y (height (frame-box-contents box)))
                       :char (frame-charmap-right-edge *frame-charmap*))
  
  ;; Blit the top side.
  (paint-horizontal-line canvas
                         y
                         (1+ x)
                         (+ x (width (frame-box-contents box)))
                         :char (frame-charmap-top-edge *frame-charmap*))
  
  ;; Blit the bottom side.
  (paint-horizontal-line canvas 
                         (+ 1 y (height (frame-box-contents box)))
                         (1+ x)
                         (+ x (width (frame-box-contents box)))
                         :char (frame-charmap-bottom-edge *frame-charmap*))
  
  ;; Blit the top-left corner.
  (setf (canvas-ref canvas x y)
        (frame-charmap-top-left-corner *frame-charmap*))
  
  ;; Blit the top-right corner.
  (setf (canvas-ref canvas
                    (+ 1 x (width (frame-box-contents box)))
                    y)
        (frame-charmap-top-right-corner *frame-charmap*))
  
  ;; Blit the bottom-left corner.
  (setf (canvas-ref canvas
                    x
                    (+ 1 y (height (frame-box-contents box))))
        (frame-charmap-bottom-left-corner *frame-charmap*))
  
  ;; Blit the bottom-right corner.
  (setf (canvas-ref canvas
                    (+ 1 x (width (frame-box-contents box)))
                    (+ 1 y (height (frame-box-contents box))))
        (frame-charmap-bottom-right-corner *frame-charmap*))
  
  ;; Blit the frame contents.
  (blit canvas (frame-box-contents box) (1+ x) (1+ y)))

;;; It is the job of the BLIT method of a ROW-BOX (and other
;;; concatenative boxes) to take care of baselines.
;;;
;;; Below is an example of boxes for a ROW-BOX where "--" is the
;;; baseline of each box.
;;;
;;; **    **
;;; ** ** **
;;; -- -- --
;;; ** **
;;;    **
(defmethod blit (canvas (box row-box) x y)
  (let ((padding (row-box-padding box))
        #+#:ignore
        (height (height box))
        #+#:ignore
        (baseline (baseline box))
        (extent (height-above-baseline box)))
    (labels ((rec (boxes x)
               (unless (null boxes)
                 (let ((the-box (first boxes)))
                   (blit canvas 
                         the-box 
                         x
                         ;; Align on the baselines.
                         (+ y (- extent (height-above-baseline the-box)))
                         #+#:ignore     ; This will vertically center
                         (+ y (floor (- height (height the-box)) 2))))
                 
                 (rec (rest boxes) (+ x padding (width (first boxes)))))))
      (rec (row-box-contents box) x))))

(defmethod blit (canvas (box picture-box) x y)
  (loop :for c :in (picture-box-picture box)
        :for i :from 0
        :do (blit canvas c x (+ y i))))

(defparameter *paren-charmap* *ascii-paren-charmap*
  "The charmap to use when drawing parentheses.")

(defmethod blit (canvas (box parens-box) x y)
  (let* ((contents (parens-box-contents box))
         (h (height box)))
    (if (= h 1)
        (blit canvas (glue (paren-charmap-small-open *paren-charmap*)
                           contents
                           (paren-charmap-small-close *paren-charmap*))
              x y)
        (progn
          ;; Blit the contents within the parentheses.
          (blit canvas contents (+ x 2) y)
          
          ;; Blit the tops of each parenthesis.
          (blit canvas
                (paren-charmap-top-open *paren-charmap*)
                x
                y)

          (blit canvas
                (paren-charmap-top-close *paren-charmap*)
                (+ x (width contents) 3)
                y)
          
          ;; Blit the centers of each parenthesis.
          (loop :for i :from 1 :below h :do 
            (blit canvas
                  (paren-charmap-middle-open *paren-charmap*)
                  x 
                  (+ y i))

            (blit canvas
                  (paren-charmap-middle-close *paren-charmap*)
                  (+ x (width contents) 3) 
                  (+ y i)))
          
          ;; Blit the bottoms of each parenthesis.
          (blit canvas
                (paren-charmap-bottom-open *paren-charmap*)
                x
                (+ y (- h 1)))

          (blit canvas
                (paren-charmap-bottom-close *paren-charmap*)
                (+ x (width contents) 3)
                (+ y (- h 1)))))))
