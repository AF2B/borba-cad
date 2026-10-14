(in-package #:borba-cad/platform)

;;;; Translation of SDL events into the application's input events. The translator reads only the
;;;; event record (plus the window for resize events), so it is tested on locally built events:
;;;; pushing text events through SDL_PushEvent crashes sdl2-compat (spike S1).

(defconstant +keymod-shift+ #x0003
  "SDL modifier mask bits for the left and right shift keys.")

(defconstant +keymod-control+ #x00C0
  "SDL modifier mask bits for the left and right control keys.")

(defconstant +keymod-alt+ #x0300
  "SDL modifier mask bits for the left and right alt keys.")

(defconstant +windowevent-exposed+ 3
  "SDL window event id: part of the window needs redrawing.")

(defconstant +windowevent-leave+ 11
  "SDL window event id: the pointer left the window.")

(defconstant +windowevent-focus-lost+ 13
  "SDL window event id: the window lost the keyboard focus.")

(defconstant +windowevent-resized+ 5
  "SDL window event id: the user resized the window.")

(defconstant +windowevent-size-changed+ 6
  "SDL window event id: the window size changed.")

(defconstant +sdl-drop-file-offset+ 8
  "Byte offset of the file name in an SDL drop event: the type and the time stamp, four bytes each,
come before it. The binding reads the name as a string and keeps the pointer to itself, but the
receiver of the event must free that pointer.")

(defconstant +sdl-key-escape+ 27 "SDL keycode of the escape key.")
(defconstant +sdl-key-return+ 13 "SDL keycode of the return key.")
(defconstant +sdl-key-backspace+ 8 "SDL keycode of the backspace key.")
(defconstant +sdl-key-delete+ 127 "SDL keycode of the delete key.")
(defconstant +sdl-key-tab+ 9 "SDL keycode of the tab key.")
(defconstant +sdl-key-space+ 32 "SDL keycode of the space bar.")
(defconstant +sdl-scancode-mask+ #x40000000 "Bit set in the SDL keycode of non-character keys.")

(defconstant +sdl-scancode-home+ 74 "SDL scancode of the home key.")
(defconstant +sdl-scancode-page-up+ 75 "SDL scancode of the page up key.")
(defconstant +sdl-scancode-end+ 77 "SDL scancode of the end key.")
(defconstant +sdl-scancode-page-down+ 78 "SDL scancode of the page down key.")
(defconstant +sdl-scancode-right+ 79 "SDL scancode of the right arrow.")
(defconstant +sdl-scancode-left+ 80 "SDL scancode of the left arrow.")
(defconstant +sdl-scancode-down+ 81 "SDL scancode of the down arrow.")
(defconstant +sdl-scancode-up+ 82 "SDL scancode of the up arrow.")
(defconstant +sdl-scancode-f1+ 58 "SDL scancode of the F1 key; F2 to F12 follow.")
(defconstant +sdl-scancode-keypad-enter+ 88 "SDL scancode of the keypad enter key.")

(defun keycode->key (keycode)
  "Return the key keyword for an SDL keycode, or NIL for keys the application ignores.

KEYCODE: an SDL keycode (layout-aware, so shortcuts follow the printed letters)."
  (cond ((<= 97 keycode 122)
         (intern (string-upcase (string (code-char keycode))) :keyword))
        ((= keycode +sdl-key-escape+) :escape)
        ((= keycode +sdl-key-return+) :enter)
        ((= keycode +sdl-key-backspace+) :backspace)
        ((= keycode +sdl-key-delete+) :delete)
        ((= keycode +sdl-key-tab+) :tab)
        ((= keycode +sdl-key-space+) :space)
        ((logtest keycode +sdl-scancode-mask+)
         (let ((scancode (logxor keycode +sdl-scancode-mask+)))
           (cond ((= scancode +sdl-scancode-home+) :home)
                 ((= scancode +sdl-scancode-end+) :end)
                 ((= scancode +sdl-scancode-page-up+) :page-up)
                 ((= scancode +sdl-scancode-page-down+) :page-down)
                 ((= scancode +sdl-scancode-left+) :left)
                 ((= scancode +sdl-scancode-right+) :right)
                 ((= scancode +sdl-scancode-up+) :up)
                 ((= scancode +sdl-scancode-down+) :down)
                 ((= scancode +sdl-scancode-keypad-enter+) :enter)
                 ((<= +sdl-scancode-f1+ scancode (+ +sdl-scancode-f1+ 11))
                  (intern (format nil "F~d" (1+ (- scancode +sdl-scancode-f1+))) :keyword))
                 (t nil))))
        (t nil)))

(defun modifier-keywords (mask)
  "Return the modifier keywords held according to an SDL modifier mask.

MASK: an SDL modifier bit mask."
  (append (and (logtest mask +keymod-shift+) '(:shift))
          (and (logtest mask +keymod-control+) '(:control))
          (and (logtest mask +keymod-alt+) '(:alt))))

(defun current-modifiers ()
  "Return the modifier keywords currently held."
  (modifier-keywords (sdl2-ffi.functions:sdl-get-mod-state)))

(defun mouse-button-keyword (button)
  "Return :left, :middle or :right for an SDL button number, or NIL.

BUTTON: SDL button number (1 left, 2 middle, 3 right)."
  (case button (1 :left) (2 :middle) (3 :right) (t nil)))

(defun current-pointer-position ()
  "Return the pointer position in window coordinates as two double-float values."
  (cffi:with-foreign-objects ((x :int) (y :int))
    (sdl2-ffi.functions:sdl-get-mouse-state x y)
    (values (float (cffi:mem-ref x :int) 1d0) (float (cffi:mem-ref y :int) 1d0))))

(defun drawable-size (window)
  "Return the OpenGL drawable size of WINDOW in physical pixels as two values.

WINDOW: an SDL window."
  (cffi:with-foreign-objects ((w :int) (h :int))
    (sdl2-ffi.functions:sdl-gl-get-drawable-size window w h)
    (values (cffi:mem-ref w :int) (cffi:mem-ref h :int))))

(defun resize-event-for (window)
  "Build a resize event from the current size and display scale of WINDOW.

WINDOW: an SDL window."
  (multiple-value-bind (width height) (sdl2:get-window-size window)
    (multiple-value-bind (drawable-width) (drawable-size window)
      (app:make-resize-event (float width 1d0)
                             (float height 1d0)
                             (if (plusp width) (/ (float drawable-width 1d0) width) 1d0)))))

(defun translate-sdl-event (event
                            window)
  "Translate one SDL event into a list of application events (possibly empty).

EVENT: an SDL event record.
WINDOW: the window, used to read the size on resize events."
  (case (sdl2:get-event-type event)
    (:mousemotion
     (list (app:make-pointer-event :move
                                   nil
                                   (float (plus-c:c-ref event sdl2-ffi:sdl-event :motion :x) 1d0)
                                   (float (plus-c:c-ref event sdl2-ffi:sdl-event :motion :y) 1d0)
                                   (current-modifiers))))
    ((:mousebuttondown :mousebuttonup)
     (let ((button (mouse-button-keyword (plus-c:c-ref event sdl2-ffi:sdl-event :button :button))))
       (and button
            (list (app:make-pointer-event (if (eq (sdl2:get-event-type event) :mousebuttondown) :down :up)
                                          button
                                          (float (plus-c:c-ref event sdl2-ffi:sdl-event :button :x) 1d0)
                                          (float (plus-c:c-ref event sdl2-ffi:sdl-event :button :y) 1d0)
                                          (current-modifiers))))))
    (:mousewheel
     (multiple-value-bind (x y) (current-pointer-position)
       (let ((notches (plus-c:c-ref event sdl2-ffi:sdl-event :wheel :y)))
         (and (/= notches 0)
              (list (app:make-wheel-event (float notches 1d0)
                                          x
                                          y
                                          (current-modifiers)))))))
    ((:keydown :keyup)
     (let ((key (keycode->key (plus-c:c-ref event sdl2-ffi:sdl-event :key :keysym :sym)))
           (repeat (/= 0 (plus-c:c-ref event sdl2-ffi:sdl-event :key :repeat))))
       (and key
            (not (and repeat (not (member key '(:backspace :delete :left :right :up :down)))))
            (list (app:make-key-event key
                                      (current-modifiers)
                                      (if (eq (sdl2:get-event-type event) :keydown) :down :up))))))
    (:textinput
     (let ((text (plus-c:c-ref event sdl2-ffi:sdl-event :text :text string)))
       (and (plusp (length text))
            (list (app:make-text-event text)))))
    (:dropfile
     (let ((pointer (cffi:mem-ref (autowrap:ptr event) :pointer +sdl-drop-file-offset+)))
       (unless (cffi:null-pointer-p pointer)
         (let ((path (cffi:foreign-string-to-lisp pointer :encoding :utf-8)))
           ;; SDL hands the path over: the receiver frees it.
           (sdl2-ffi.functions:sdl-free pointer)
           (and (plusp (length path))
                (list (app:make-file-drop-event path)))))))
    (:windowevent
     (let ((kind (plus-c:c-ref event sdl2-ffi:sdl-event :window :event)))
       (cond ((member kind (list +windowevent-resized+ +windowevent-size-changed+))
              (list (resize-event-for window)))
             ((= kind +windowevent-exposed+) (list :redraw))
             ((= kind +windowevent-focus-lost+) (list (app:make-window-state-event :focus-lost)))
             ((= kind +windowevent-leave+) (list (app:make-window-state-event :pointer-left)))
             (t nil))))
    (:quit (list (app:make-quit-event)))
    (t nil)))
