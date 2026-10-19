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

(defconstant +reference-window-width+ 1100d0
  "Width, in logical pixels, that the layout is designed to: a window narrower than this has the interface drawn
smaller, so that the layout still has this much room.")

(defconstant +reference-window-height+ 640d0
  "Height, in logical pixels, that the layout is designed to.")

(defconstant +minimum-interface-scale+ 0.6d0
  "Smallest the interface is ever drawn, as a fraction of its size: below it the text would be too small to read.")

(defvar *interface-zoom* 1d0
  "The zoom of the interface the user asked for, as the application keeps it.")

(defvar *interface-scale* 1d0
  "How much the interface is drawn larger (above 1) or smaller (below 1) than the layout is designed to, as
INTERFACE-SCALE-FOR worked it out for the window as it is now. Positions of the pointer are divided by it, and the
size of the window is divided by it to give the size the application lays out.")

(defun interface-scale-for (width
                            height
                            zoom)
  "Return how large to draw the interface in a window: normally as designed, smaller when the window is smaller than
the layout is designed to (so that nothing is cut off or overlaps), and larger by ZOOM when the window has room for it.
Never below +MINIMUM-INTERFACE-SCALE+.

WIDTH: window width in logical pixels.
HEIGHT: window height in logical pixels.
ZOOM: the zoom the user asked for; 1 is as designed."
  (let* ((fit (min (/ (float width 1d0) +reference-window-width+)
                   (/ (float height 1d0) +reference-window-height+)))
         (asked (* zoom (min fit 1d0))))
    (max +minimum-interface-scale+
         (min fit asked))))

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

(defconstant +sdl-key-minus+ 45 "SDL keycode of the minus key.")
(defconstant +sdl-key-plus+ 43 "SDL keycode of the plus character.")
(defconstant +sdl-key-equals+ 61 "SDL keycode of the equals key, which shares a key with the plus on many layouts.")
(defconstant +sdl-key-zero+ 48 "SDL keycode of the zero key.")
(defconstant +sdl-scancode-keypad-minus+ 86 "SDL scancode of the minus of the numeric keypad.")
(defconstant +sdl-scancode-keypad-plus+ 87 "SDL scancode of the plus of the numeric keypad.")
(defconstant +sdl-scancode-keypad-zero+ 98 "SDL scancode of the zero of the numeric keypad.")

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

(defun zoom-keycode->key (keycode)
  "Return :minus, :plus or :zero for the SDL keycode of a key that zooms the interface when Control is held, or NIL.

KEYCODE: an SDL keycode."
  (flet ((keypad (scancode)
           (and (logtest keycode +sdl-scancode-mask+)
                (= scancode (logxor keycode +sdl-scancode-mask+)))))
    (cond ((or (= keycode +sdl-key-minus+) (keypad +sdl-scancode-keypad-minus+)) :minus)
          ((or (= keycode +sdl-key-plus+) (= keycode +sdl-key-equals+) (keypad +sdl-scancode-keypad-plus+)) :plus)
          ((or (= keycode +sdl-key-zero+) (keypad +sdl-scancode-keypad-zero+)) :zero)
          (t nil))))

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
  "Return the pointer position in the coordinates the application lays out, as two double-float values."
  (cffi:with-foreign-objects ((x :int) (y :int))
    (sdl2-ffi.functions:sdl-get-mouse-state x y)
    (values (/ (float (cffi:mem-ref x :int) 1d0) *interface-scale*)
            (/ (float (cffi:mem-ref y :int) 1d0) *interface-scale*))))

(defun drawable-size (window)
  "Return the OpenGL drawable size of WINDOW in physical pixels as two values.

WINDOW: an SDL window."
  (cffi:with-foreign-objects ((w :int) (h :int))
    (sdl2-ffi.functions:sdl-gl-get-drawable-size window w h)
    (values (cffi:mem-ref w :int) (cffi:mem-ref h :int))))

(defun logical-window-size (window)
  "Return the size of WINDOW in the coordinates the application lays out, as two double-float values: the size of the
window divided by the scale of the interface, which is worked out again here.

WINDOW: an SDL window."
  (multiple-value-bind (width height) (sdl2:get-window-size window)
    (setf *interface-scale* (interface-scale-for width
                                                 height
                                                 *interface-zoom*))
    (values (/ (float width 1d0) *interface-scale*)
            (/ (float height 1d0) *interface-scale*))))

(defun resize-event-for (window)
  "Build a resize event from the current size of WINDOW, in the coordinates the application lays out, and the number
of physical pixels there are to each of those.

WINDOW: an SDL window."
  (multiple-value-bind (width height) (logical-window-size window)
    (multiple-value-bind (drawable-width) (drawable-size window)
      (app:make-resize-event width
                             height
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
                                   (/ (float (plus-c:c-ref event sdl2-ffi:sdl-event :motion :x) 1d0)
                                      *interface-scale*)
                                   (/ (float (plus-c:c-ref event sdl2-ffi:sdl-event :motion :y) 1d0)
                                      *interface-scale*)
                                   (current-modifiers))))
    ((:mousebuttondown :mousebuttonup)
     (let ((button (mouse-button-keyword (plus-c:c-ref event sdl2-ffi:sdl-event :button :button))))
       (and button
            (list (app:make-pointer-event (if (eq (sdl2:get-event-type event) :mousebuttondown) :down :up)
                                          button
                                          (/ (float (plus-c:c-ref event sdl2-ffi:sdl-event :button :x) 1d0)
                                             *interface-scale*)
                                          (/ (float (plus-c:c-ref event sdl2-ffi:sdl-event :button :y) 1d0)
                                             *interface-scale*)
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
     (let* ((keycode (plus-c:c-ref event sdl2-ffi:sdl-event :key :keysym :sym))
            (key (or (keycode->key keycode)
                     (and (member :control (current-modifiers))
                          (zoom-keycode->key keycode))))
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
