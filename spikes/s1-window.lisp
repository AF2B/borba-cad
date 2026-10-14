;;;; Spike S1 (throwaway): window + GL context + input events + 10k line segments via CFFI.
;;;; Run from the project root:
;;;;   sbcl --dynamic-space-size 4GB --noinform --non-interactive --no-sysinit --no-userinit \
;;;;        --load ~/quicklisp/setup.lisp --load spikes/s1-window.lisp

(require :sb-posix)
(ql:quickload '(:sdl2 :cl-opengl) :silent t)
(load "spikes/png.lisp")

(defpackage #:s1 (:use #:cl))
(in-package #:s1)

(defun mono-ns ()
  (multiple-value-bind (sec nsec) (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* sec 1000000000) nsec)))

(defparameter *vertex-shader* "#version 330 core
layout(location=0) in vec2 pos;
layout(location=1) in vec4 col;
uniform vec2 viewport;
out vec4 v_col;
void main() {
  vec2 ndc = vec2(pos.x / viewport.x * 2.0 - 1.0, 1.0 - pos.y / viewport.y * 2.0);
  gl_Position = vec4(ndc, 0.0, 1.0);
  v_col = col;
}")

(defparameter *fragment-shader* "#version 330 core
in vec4 v_col;
out vec4 frag;
void main() { frag = v_col; }")

(defun compile-shader (type source)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    (unless (gl:get-shader shader :compile-status)
      (error "Shader compile failed: ~a" (gl:get-shader-info-log shader)))
    shader))

(defun make-program ()
  (let ((program (gl:create-program)))
    (gl:attach-shader program (compile-shader :vertex-shader *vertex-shader*))
    (gl:attach-shader program (compile-shader :fragment-shader *fragment-shader*))
    (gl:link-program program)
    (unless (gl:get-program program :link-status)
      (error "Program link failed: ~a" (gl:get-program-info-log program)))
    program))

(defconstant +segments+ 10000)
(defconstant +floats-per-vertex+ 6)

(defun fill-vertices (vertices time width height)
  "Fill VERTICES with +SEGMENTS+ line segments drifting with TIME (simulates a pan)."
  (declare (type (simple-array single-float (*)) vertices)
           (type single-float time width height)
           (optimize (speed 3) (safety 0)))
  (let ((offset (* 40.0 (sin time)))
        (index 0))
    (declare (type single-float offset) (type fixnum index))
    (dotimes (i +segments+)
      (let* ((fx (/ (float (mod (* i 7919) 1000) 1.0) 1000.0))
             (fy (/ (float (mod (* i 104729) 1000) 1.0) 1000.0))
             (x1 (+ (* fx width) offset))
             (y1 (* fy height))
             (x2 (+ x1 (* 30.0 (- fx 0.5))))
             (y2 (+ y1 (* 30.0 (- fy 0.5)))))
        (declare (type single-float fx fy x1 y1 x2 y2))
        (setf (aref vertices index) x1
              (aref vertices (+ index 1)) y1
              (aref vertices (+ index 2)) 0.2
              (aref vertices (+ index 3)) 0.8
              (aref vertices (+ index 4)) 1.0
              (aref vertices (+ index 5)) 1.0
              (aref vertices (+ index 6)) x2
              (aref vertices (+ index 7)) y2
              (aref vertices (+ index 8)) 0.2
              (aref vertices (+ index 9)) 0.8
              (aref vertices (+ index 10)) 1.0
              (aref vertices (+ index 11)) 1.0)
        (incf index 12)))
    index))

(defun percentile (sorted p)
  (aref sorted (min (1- (length sorted)) (floor (* p (length sorted))))))

(defun report-stats (name samples)
  (let ((sorted (sort (coerce samples 'vector) #'<)))
    (format t "~12a p50=~,3f ms  p95=~,3f ms  max=~,3f ms~%" name
            (/ (percentile sorted 0.5) 1e6) (/ (percentile sorted 0.95) 1e6) (/ (aref sorted (1- (length sorted))) 1e6))))

(defun drawable-size (window)
  "Return the GL drawable size in framebuffer pixels (differs from window size under HiDPI)."
  (cffi:with-foreign-objects ((w :int) (h :int))
    (sdl2-ffi.functions:sdl-gl-get-drawable-size window w h)
    (values (cffi:mem-ref w :int) (cffi:mem-ref h :int))))

(defun save-screenshot (width height path)
  (let ((pixels (gl:read-pixels 0 0 width height :rgb :unsigned-byte)))
    (spike-png:write-rgb-png path width height pixels)
    (format t "screenshot: ~a (~dx~d)~%" path width height)))

(defun make-text-event (&rest code-bytes)
  "Build an SDL text-input event whose payload is the given UTF-8 bytes (not pushed)."
  (let ((ev (sdl2:new-event :textinput)))
    (loop for byte in (append code-bytes '(0))
          for i from 0
          do (setf (plus-c:c-ref ev sdl2-ffi:sdl-event :text :text i)
                   (if (> byte 127) (- byte 256) byte)))
    ev))

(defun decode-test ()
  "Decode a locally built event (no SDL queue involved): are UTF-8 bytes decoded correctly?"
  (let* ((ev (make-text-event #xC3 #xA9 #xC3 #xA7 #x61))
         (text (plus-c:c-ref ev sdl2-ffi:sdl-event :text :text string)))
    (format t "decode test: ~s -> code points ~a (expected (233 231 97))~%" text (map 'list #'char-code text))
    (sdl2:free-event ev)))

(defun push-synthetic-text (window)
  "Push a text event with a valid window id through the real SDL queue."
  (let ((ev (make-text-event #xC3 #xA9 #xC3 #xA7)))
    (setf (plus-c:c-ref ev sdl2-ffi:sdl-event :text :window-id) (sdl2:get-window-id window)
          (plus-c:c-ref ev sdl2-ffi:sdl-event :text :timestamp) (sdl2:get-ticks))
    (sdl2:push-event ev)
    (sdl2:free-event ev)))

(defun make-wheel-event (window)
  (let ((ev (sdl2:new-event :mousewheel)))
    (setf (plus-c:c-ref ev sdl2-ffi:sdl-event :wheel :window-id) (sdl2:get-window-id window)
          (plus-c:c-ref ev sdl2-ffi:sdl-event :wheel :timestamp) (sdl2:get-ticks)
          (plus-c:c-ref ev sdl2-ffi:sdl-event :wheel :y) 1)
    ev))

(defun run (&key (frames 400) push-test)
  (let ((received-text nil)
        (frame-cpu '()) (frame-swap '()) (frame-interval '())
        (vertices (make-array (* +segments+ 2 +floats-per-vertex+) :element-type 'single-float))
        (last-present nil))
    (sdl2:with-init (:video)
      (format t "video driver: ~a~%" (sdl2:get-current-video-driver))
      (sdl2:gl-set-attr :context-major-version 3)
      (sdl2:gl-set-attr :context-minor-version 3)
      (sdl2:gl-set-attr :context-profile-mask sdl2-ffi:+sdl-gl-context-profile-core+)
      (sdl2:gl-set-attr :doublebuffer 1)
      (sdl2:gl-set-attr :multisamplebuffers 1)
      (sdl2:gl-set-attr :multisamplesamples 4)
      (sdl2:with-window (window :title "Borba CAD S1" :w 1280 :h 800 :flags '(:shown :opengl :resizable :allow-highdpi))
        (sdl2:with-gl-context (context window)
          (sdl2:gl-make-current window context)
          (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl2:gl-get-proc-address)
          (sdl2:gl-set-swap-interval 1)
          (format t "GL: ~a | ~a | ~a~%" (gl:get-string :vendor) (gl:get-string :renderer) (gl:get-string :version))
          (multiple-value-bind (dw dh) (drawable-size window)
            (multiple-value-bind (ww wh) (sdl2:get-window-size window)
              (format t "window ~dx~d, drawable ~dx~d~%" ww wh dw dh)))
          (sdl2:start-text-input)
          (decode-test)
          (let* ((program (make-program))
                 (vao (gl:gen-vertex-array))
                 (vbo (gl:gen-buffer))
                 (viewport-location (gl:get-uniform-location program "viewport")))
            (gl:bind-vertex-array vao)
            (gl:bind-buffer :array-buffer vbo)
            (%gl:buffer-data :array-buffer (* 4 (length vertices)) (cffi:null-pointer) :stream-draw)
            (gl:vertex-attrib-pointer 0 2 :float nil 24 0)
            (gl:enable-vertex-attrib-array 0)
            (gl:vertex-attrib-pointer 1 4 :float nil 24 8)
            (gl:enable-vertex-attrib-array 1)
            (gl:enable :multisample)
            (dotimes (frame frames)
              (let ((t0 (mono-ns)))
                ;; events
                (when (and push-test (= frame 5))
                  (let ((ev (make-wheel-event window)))
                    (sdl2:push-event ev)
                    (sdl2:free-event ev)))
                (sdl2:with-sdl-event (event)
                  (loop while (/= 0 (sdl2-ffi.functions:sdl-poll-event event))
                        do (case (sdl2:get-event-type event)
                             (:textinput
                              (setf received-text (plus-c:c-ref event sdl2-ffi:sdl-event :text :text string)))
                             (:mousewheel
                              (setf received-text (list :wheel-dy (plus-c:c-ref event sdl2-ffi:sdl-event :wheel :y))))
                             (:quit (return-from run :quit)))))
                ;; render
                (multiple-value-bind (w h) (drawable-size window)
                  (gl:viewport 0 0 w h)
                  (gl:clear-color 0.08 0.09 0.11 1.0)
                  (gl:clear :color-buffer-bit)
                  (gl:use-program program)
                  (gl:uniformf viewport-location (float w 1.0) (float h 1.0))
                  (fill-vertices vertices (* frame 0.02) (float w 1.0) (float h 1.0))
                  (cffi:with-pointer-to-vector-data (ptr vertices)
                    (%gl:buffer-sub-data :array-buffer 0 (* 4 (length vertices)) ptr))
                  (gl:draw-arrays :lines 0 (* 2 +segments+))
                  (when (= frame 30)
                    (save-screenshot w h "/home/borba/Documents/borba-cad/spikes/out/s1.png")))
                (let ((t1 (mono-ns)))
                  (sdl2:gl-swap-window window)
                  (let ((t2 (mono-ns)))
                    (when (> frame 10)
                      (push (- t1 t0) frame-cpu)
                      (push (- t2 t1) frame-swap)
                      (when last-present (push (- t2 last-present) frame-interval)))
                    (setf last-present t2)))))
            (gl:delete-buffers (list vbo))
            (gl:delete-vertex-arrays (list vao))))))
    (format t "received text event: ~s (code points ~a)~%" received-text (map 'list #'char-code (or received-text "")))
    (format t "~%frame stats over ~d frames, ~d line segments per frame:~%" (length frame-cpu) +segments+)
    (report-stats "cpu (ev+build+upload+draw)" frame-cpu)
    (report-stats "swap (incl. vsync)" frame-swap)
    (report-stats "interval" frame-interval)))

(handler-bind ((arithmetic-error
                 (lambda (c) (format t "~%FLOAT TRAP: ~a~%" c) (sb-debug:print-backtrace :count 12))))
  (run :frames (if (sb-posix:getenv "S1_PUSH_TEST") 20 400) :push-test (sb-posix:getenv "S1_PUSH_TEST")))
(format t "~%S1 finished OK~%")
