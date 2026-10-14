;;;; Spike S4 (throwaway): can a saved SBCL executable reopen SDL/GL/FreeType at startup?
;;;; Build: sbcl --dynamic-space-size 2GB --noinform --non-interactive --no-sysinit --no-userinit \
;;;;             --load ~/quicklisp/setup.lisp --load spikes/s4-build.lisp
(require :sb-posix)
(ql:quickload '(:sdl2 :cl-opengl :cl-freetype2) :silent t)

(defpackage #:s4 (:use #:cl) (:export #:main))
(in-package #:s4)

(defun mono-ns ()
  (multiple-value-bind (sec nsec) (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* sec 1000000000) nsec)))

(defun main ()
  (let ((t0 (mono-ns)))
    (format t "S4 argv: ~s~%" sb-ext:*posix-argv*)
    (sdl2:with-init (:video)
      (sdl2:gl-set-attr :context-major-version 3)
      (sdl2:gl-set-attr :context-minor-version 3)
      (sdl2:gl-set-attr :context-profile-mask sdl2-ffi:+sdl-gl-context-profile-core+)
      (sdl2:with-window (window :title "Borba CAD S4" :w 640 :h 360 :flags '(:shown :opengl))
        (sdl2:with-gl-context (context window)
          (sdl2:gl-make-current window context)
          (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl2:gl-get-proc-address)
          (dotimes (i 30)
            (gl:clear-color 0.1 0.2 0.3 1.0)
            (gl:clear :color-buffer-bit)
            (sdl2:gl-swap-window window)
            (when (= i 0)
              (format t "first frame presented ~,1f ms after main entry~%" (/ (- (mono-ns) t0) 1e6))))
          (format t "renderer: ~a | driver: ~a~%" (gl:get-string :renderer) (sdl2:get-current-video-driver))
          ;; cl-freetype2 creates *LIBRARY* at load time; that handle is dangling in a saved image,
          ;; so the application creates its own at startup.
          (let ((library (freetype2:make-freetype)))
            (freetype2:with-open-face (face "/usr/share/fonts/noto/NotoSansMono-Regular.ttf" 0 library)
              (freetype2:set-pixel-sizes face 0 16)
              (format t "freetype face opened: ascender ~a px~%" (freetype2:face-ascender-pixels face)))))))
    (format t "S4 OK (dynamic-space ~d MiB)~%" (floor (sb-ext:dynamic-space-size) 1048576))
    (finish-output)
    (sb-ext:exit :code 0)))

(setf freetype2:*library* nil)
(sb-ext:gc :full t)

(sb-ext:save-lisp-and-die "/home/borba/Documents/borba-cad/spikes/out/s4-exe"
                          :executable t
                          :toplevel #'s4:main
                          :save-runtime-options t)
