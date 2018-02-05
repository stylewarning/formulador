;;;; package.lisp
;;;;
;;;; Copyright (c) 2011-2018 Robert Smith

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
   #:*globally-disable-dimensions-caching* ; DYNAMIC VARIABLE
   #:width
   #:height
   #:baseline

   #:box                                ; CLASS, GENERIC, METHODS
   #:empty-box
   #:glass-box
   #:frozen-box
   #:freeze                             ; FUNCTION
   #:phantom-box                        ; CLASS
   #:phantom                            ; FUNCTION
   #:string-box
   #:frac-box
   #:*frac-box-vinculum-padding*
   #:frame-box
   #:row-box
   #:picture-box
   #:limits-box
   #:sqrt-box
   #:script-box
   #:parens-box)

  ;; blit.lisp
  (:export
   #:blit                               ; GENERIC, METHODS
   )

  ;; render.lisp
  (:export
   #:draw)

  ;; constructions.lisp
  (:export
   #:glue
   #:tape
   #:+center-dot+
   #:+cdots+
   #:+partial+
   #:+sigma+
   #:+integral+
   #:+double-integral+
   #:+triple-integral+)

  (:documentation "Formula renderering package."))
