;;;; blit.lisp
;;;; Copyright (c) 2013 Robert Smith

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
            :do (setf (canvas-ref canvas i mid-y) #\-)))))

(defmethod blit (canvas (box frame-box) x y)
  
  ;; left side
  (paint-vertical-line canvas
                       x
                       (1+ y)
                       (+ y (height (frame-box-contents box))))
  
  ;; right side
  (paint-vertical-line canvas 
                       (+ 1 x (width (frame-box-contents box)))
                       (1+ y)
                       (+ y (height (frame-box-contents box))))
  
  ;; top side
  (paint-horizontal-line canvas
                         y
                         (1+ x)
                         (+ x (width (frame-box-contents box))))
  
  ;; bottom side
  (paint-horizontal-line canvas 
                         (+ 1 y (height (frame-box-contents box)))
                         (1+ x)
                         (+ x (width (frame-box-contents box))))
  
  ;; top-left corner
  (setf (canvas-ref canvas x y) #\+)
  
  ;; top-right corner
  (setf (canvas-ref canvas
                    (+ 1 x (width (frame-box-contents box)))
                    y)
        #\+)
  
  ;; bottom-left corner
  (setf (canvas-ref canvas
                    x
                    (+ 1 y (height (frame-box-contents box))))
        #\+)
  
  ;; bottom-right corner
  (setf (canvas-ref canvas
                    (+ 1 x (width (frame-box-contents box)))
                    (+ 1 y (height (frame-box-contents box))))
        #\+)
  
  ;; frame contents
  (blit canvas (frame-box-contents box) (1+ x) (1+ y)))

(defmethod blit (canvas (box row-box) x y)
  (let ((padding (row-box-padding box))
        (height (height box)))
    (labels ((rec (boxes x)
               (unless (null boxes)
                 (let ((the-box (first boxes)))
                   (blit canvas 
                         the-box 
                         x
                         (+ y (floor (- height (height the-box)) 2))))
                 
                 (rec (rest boxes) (+ x padding (width (first boxes)))))))
      (rec (row-box-contents box) x))))

(defmethod blit (canvas (box picture-box) x y)
  (loop :for c :in (picture-box-picture box)
        :for i :from 0
        :do (blit canvas c x (+ y i))))

(defmethod blit (canvas (box parens-box) x y)
  (let* ((contents (parens-box-contents box))
	 (h (height contents)))
    (if (= h 1)
	(blit canvas (glue #\( contents #\)) x y)
	(progn
	  (blit canvas contents (+ x 2) y)
	  (blit canvas #\/  x y)
	  (blit canvas #\\ (+ x (width contents) 3) y)
	  (loop :for i :from 1 :below h
		:do (progn
		      (blit canvas #\| x (+ y i))
		      (blit canvas #\| (+ x (width contents) 3) (+ y i))))
	  (blit canvas #\\ x (+ y (- h 1)))
	  (blit canvas #\/  (+ x (width contents) 3) (+ y (- h 1)))))))
