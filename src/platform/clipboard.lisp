(in-package #:borba-cad/platform)

;;;; The system clipboard, through SDL. The application asks for it through two functions it is given.

(defun sdl-clipboard-text ()
  "Return the text on the system clipboard, or NIL when there is none or it is empty. SDL gives
the text in memory of its own, which is freed here."
  (let ((pointer (cffi:foreign-funcall "SDL_GetClipboardText" :pointer)))
    (unless (cffi:null-pointer-p pointer)
      (unwind-protect
           (let ((text (cffi:foreign-string-to-lisp pointer :encoding :utf-8)))
             (and (plusp (length text))
                  text))
        (sdl2-ffi.functions:sdl-free pointer)))))

(defun set-sdl-clipboard-text (text)
  "Put TEXT on the system clipboard. Returns true when SDL accepted it.

TEXT: the string to copy."
  (cffi:with-foreign-string (foreign text :encoding :utf-8)
    (zerop (cffi:foreign-funcall "SDL_SetClipboardText" :pointer foreign :int))))

(defun install-system-clipboard (app)
  "Make the application use the system clipboard.

APP: the application."
  (setf (app:app-clipboard-reader app) #'sdl-clipboard-text
        (app:app-clipboard-writer app) #'set-sdl-clipboard-text))
