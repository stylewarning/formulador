;;;; package.lisp
;;;; Copyright (c) 2011-2013 Robert Smith

;;;; Declare the formulador package.

(defpackage #:formulador
  (:use #:cl)
  
  ;; canvas.lisp
  (:export
   #:canvas
   #:canvasp
   #:make-canvas
   #:canvas-dimensions
   #:canvas-ref)

  ;; boxes.lisp
  (:export
   #:width                              ; export?
   #:height                             ; export?
   
   #:box                                ; export predicates?
   #:empty-box
   #:string-box
   #:frac-box
   #:*frac-box-vinculum-padding*
   #:frame-box
   #:row-box)
  
  ;; blit.lisp
  ;; Do we really want to export this junk?
  (:export
   #:blit)
  
  ;; render.lisp
  (:export
   #:draw)
  
  (:documentation "Formula renderering package."))
