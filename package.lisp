;;;; package.lisp
;;;;
;;;; Copyright (c) 2011-2018 Robert Smith

(defpackage #:formulador
  (:use #:cl)

  ;; canvas.lisp
  (:export
   #:canvas                             ; TYPE/STRUCTURE
   #:canvasp
   #:make-canvas
   #:canvas-dimensions
   #:*error-on-out-of-bounds-write*     ; VARIABLE
   #:*warn-on-out-of-bounds-write*      ; VARIABLE
   #:canvas-ref                         ; FUNCTION, SETF
   )

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
   #:hphantom-box                       ; CLASS
   #:hphantom                           ; FUNCTION
   #:vphantom-box                       ; CLASS
   #:vphantom                           ; FUNCTION
   #:overlap-box                        ; ABSTRACT CLASS
   #:llap-box                           ; CLASS
   #:llap                               ; FUNCTION
   #:clap-box                           ; CLASS
   #:clap                               ; FUNCTION
   #:rlap-box                           ; CLASS
   #:rlap                               ; FUNCTION
   #:string-box
   #:frac-box
   #:*frac-box-vinculum-padding*
   #:frame-box
   #:vertical-alignment
   #:row-box
   #:horizontal-alignment
   #:column-box
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
