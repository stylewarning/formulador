;;;; editor.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:formulador-editor)

;;; EXAMPLE FORMULA

(defparameter *formula*
  (flet ((pow (a b)
           (script-box a :superscript b)))
    (let ((pi-letter (box (code-char #x3C0))))
      (tape
       (frac-box (box "1") pi-letter)
       (box "=")
       (frac-box (sqrt-box (pow (box "640320")
                                (box "3")))
                 (box "12"))
       (limits-box +sigma+
                   :above (box "∞")
                   :below (box "k = 0"))
       (frac-box (tape (box "(6k)!")
                       (box "(545140134k + 13591409)"))
                 (tape (box "(3k)!")
                       (pow (box "(k!)")
                            (box "3"))))
       (pow (parens-box
             (tape (box "-") (frac-box (box "1")
                                       (pow (box "640320")
                                            (box "3")))))
            (box "k"))))))

(defparameter *canvas* (formulador:draw *formula*))

;;; EDITOR

(defvar *editor-initialized* nil)

(defun check-editor-initialized ()
  (unless *editor-initialized*
    (error "The editor is not initialized.")))

(defun refresh-editor ()
  (check-editor-initialized)
  (charms:refresh-window charms:*standard-window*))

(defun move-cursor (x y)
  (check-editor-initialized)
  (charms:move-cursor charms:*standard-window* x y)
  (refresh-editor))

(defun printablep (char)
  (and (graphic-char-p char)
       (<= 0 (char-code char) 127)))

(defun render-formula (window x0 y0)
  (let ((width (formulador:width *formula*))
        (height (formulador:height *formula*))
        (canvas (formulador:draw *formula*)))
    (dotimes (y height)
      (dotimes (x width)
        (let ((char (canvas-ref canvas x y)))
          (charms:write-char-at-point window
                                      (if (printablep char)
                                          char
                                          #\?)
                                      (+ x x0)
                                      (+ y y0)))))))

(defun highlight-region (window region)
  (loop :for y :from (formulador::region-min-y region)
          :below (formulador::region-max-y region)
        :do (loop :for x :from (formulador::region-min-x region)
                    :below (formulador::region-max-x region)
                  :for c := (charms:char-at-point window x y)
                  :do (charms/ll:mvwaddch (charms::window-pointer window) y x (logior (char-code c) charms/ll:A_STANDOUT)))))

(defun start-editor ()
  ;; Initialize ncurses.
  (charms:with-curses ()
    (let ((*editor-initialized* t))
      ;; Do not echo characters immediately. We will do this
      ;; ourselves.
      (charms:disable-echoing)

      ;; Do not buffer.
      (charms:enable-raw-input :interpret-control-characters t)

      ;; Enable non-blocking.
      (charms:enable-non-blocking-mode charms:*standard-window*)

      ;; Enable function keys, arrow keys, etc.
      (charms:enable-extra-keys charms:*standard-window*)

      ;; Wait for a character
      (loop :named driver-loop
            :with regions := (formulador::find-associations *canvas* 0 0)
            :with level := 0
            :for c := (charms:get-char charms:*standard-window* :ignore-error t)
            :do (labels ((cursor-moved ()
                           (multiple-value-bind (cursor-x cursor-y)
                               (charms:cursor-position charms:*standard-window*)
                             (setf regions
                                   (formulador::find-associations *canvas*
                                                                  cursor-x
                                                                  cursor-y))
                             (setf level 0)))
                         (descend ()
                           (setf level (mod (1+ level) (length regions))))
                         (highlight-level ()
                           (when regions
                             (highlight-region charms:*standard-window*
                                               (car (elt regions level))))))
                  (charms:with-restored-cursor charms:*standard-window*
                    (render-formula charms:*standard-window* 0 0)
                    (highlight-level))

                  (cond
                    ((null c) nil)
                    ((char= c #\/)
                     (descend))
                    ((char= c #\f)
                     (charms:beep-console)
                     (charms:flash-console))
                    ((char= c (code-char charms/ll:KEY_UP))
                     (charms:move-cursor-up charms:*standard-window*)
                     (cursor-moved))
                    ((char= c (code-char charms/ll:KEY_DOWN))
                     (charms:move-cursor-down charms:*standard-window*)
                     (cursor-moved))
                    ((char= c (code-char charms/ll:KEY_LEFT))
                     (charms:move-cursor-left charms:*standard-window*)
                     (cursor-moved))
                    ((char= c (code-char charms/ll:KEY_RIGHT))
                     (charms:move-cursor-right charms:*standard-window*)
                     (cursor-moved))
                    (t (return-from driver-loop)))

                  ;; Refresh the screen.
                  (refresh-editor))))))
