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

(defun render-formula (scr x0 y0)
  (let ((width (formulador:width *formula*))
        (height (formulador:height *formula*))
        (canvas (formulador:draw *formula*)))
    (dotimes (y height)
      (dotimes (x width)
        (let ((char (canvas-ref canvas x y)))
          (charms/ll:mvwaddch scr
                              (+ y y0)
                              (+ x x0)
                              (char-code (if (printablep char)
                                             char
                                             #\?))))))))

(defun highlight-region (window region)
  (loop :for y :from (formulador::region-min-y region)
          :below (formulador::region-max-y region)
        :do (loop :for x :from (formulador::region-min-x region)
                    :below (formulador::region-max-x region)
                  :for c := (charms/ll:mvwinch window y x)
                  :do (charms/ll:mvwaddch window y x (logior c charms/ll:A_STANDOUT)))))

(defun start-editor ()
  ;; Initialize ncurses.
  (charms:with-curses ()
    (let ((*editor-initialized* t))
      ;; Do not echo characters immediately. We will do this
      ;; ourselves.
      (charms:disable-echoing)
      
      ;; Do not buffer.
      (charms:enable-raw-input :interpret-control-characters t)
      
      ;; Disable blocking.
      (charms/ll:nodelay charms/ll:*stdscr* charms/ll:TRUE)
      
      ;; Enable function keys, arrow keys, etc.
      (charms:enable-extra-keys charms:*standard-window*)
      
      ;; Wait for a character
      (loop :named driver-loop
            :with regions := (formulador::find-associations *canvas* 0 0)
            :with level := 0
            :for c := (charms/ll:wgetch charms/ll:*stdscr*)
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
                             (highlight-region charms/ll:*stdscr*
                                               (car (elt regions level))))))
                  (charms:with-restored-cursor charms:*standard-window*
                    (render-formula (charms::window-pointer
                                     charms:*standard-window*) 0 0)
                    (highlight-level))                     
                  
                  (cond
                    ((= c (char-code #\/))
                     (descend))
                    ((= c charms/ll:KEY_UP)
                     (charms:move-cursor-up charms:*standard-window*)
                     (cursor-moved))
                    ((= c charms/ll:KEY_DOWN)
                     (charms:move-cursor-down charms:*standard-window*)
                     (cursor-moved))
                    ((= c charms/ll:KEY_LEFT)
                     (charms:move-cursor-left charms:*standard-window*)
                     (cursor-moved))
                    ((= c charms/ll:KEY_RIGHT)
                     (charms:move-cursor-right charms:*standard-window*)
                     (cursor-moved))
                    ((= c charms/ll:ERR) nil)
                    (t (return-from driver-loop)))
                  
                  ;; Refresh the screen.
                  (refresh-editor))))))
