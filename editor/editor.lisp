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
  (charms:wrefresh charms:*stdscr*))

(defun move-cursor (x y)
  (check-editor-initialized)
  (charms:wmove charms:*stdscr* y x)
  (charms:wrefresh charms:*stdscr*))

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
          (charms:mvwaddch scr
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
                  :for c := (charms:mvwinch window y x)
                  :do (charms:mvwaddch window y x (logior c charms:A_STANDOUT)))))

(defun start-editor ()
  ;; Clear any output that hasn't been written.
  (force-output *terminal-io*)
  
  ;; Initialize ncurses.
  (charms:initscr)
  (unwind-protect
       (let ((*editor-initialized* t))
         ;; Do not echo characters immediately. We will do this
         ;; ourselves.
         (charms:noecho)
         
         ;; Do not buffer.
         (charms:cbreak)
         
         ;; Disable blocking.
         (charms:nodelay charms:*stdscr* charms:TRUE)
         
         ;; Enable function keys, arrow keys, etc.
         (charms:keypad charms:*stdscr* charms:TRUE)
         
         ;; Wait for a character
         (loop :named driver-loop
               :with regions := (formulador::find-associations *canvas* 0 0)
               :with level := 0
               :for c := (charms:wgetch charms:*stdscr*)
               :do (labels ((cursor-moved ()
                              (multiple-value-bind (cursor-x cursor-y)
                                  (cursor-position charms:*stdscr*)
                                (setf regions
                                      (formulador::find-associations *canvas*
                                                                     cursor-x
                                                                     cursor-y))
                                (setf level 0)))
                            (descend ()
                              (setf level (mod (1+ level) (length regions))))
                            (highlight-level ()
                              (when regions
                                (highlight-region charms:*stdscr*
                                                  (car (elt regions level))))))
                     (with-restored-cursor charms:*stdscr*
                       (render-formula charms:*stdscr* 0 0)
                       (highlight-level))                     

                     (cond
                       ((= c (char-code #\/))
                        (descend))
                       ((= c charms:KEY_UP)
                        (cursor-up charms:*stdscr*)
                        (cursor-moved))
                       ((= c charms:KEY_DOWN)
                        (cursor-down charms:*stdscr*)
                        (cursor-moved))
                       ((= c charms:KEY_LEFT)
                        (cursor-left charms:*stdscr*)
                        (cursor-moved))
                       ((= c charms:KEY_RIGHT)
                        (cursor-right charms:*stdscr*)
                        (cursor-moved))
                       ((= c charms:ERR) nil)
                       (t (return-from driver-loop)))
                     
                     ;; Refresh the screen.
                     (charms:wrefresh charms:*stdscr*))))

    ;; Finalize ncurses.
    (charms:endwin)))
