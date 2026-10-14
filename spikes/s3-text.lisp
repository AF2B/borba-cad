;;;; Spike S3 (throwaway): UTF-8 text through FreeType into a GL glyph atlas, at several scales.
;;;; Run from the project root:
;;;;   sbcl --dynamic-space-size 2GB --noinform --non-interactive --no-sysinit --no-userinit \
;;;;        --load ~/quicklisp/setup.lisp --load spikes/s3-text.lisp

(require :sb-posix)
(ql:quickload '(:sdl2 :cl-opengl :cl-freetype2) :silent t)
(load "spikes/png.lisp")

(defpackage #:s3 (:use #:cl))
(in-package #:s3)

(defparameter *font-path* "/usr/share/fonts/noto/NotoSansMono-Regular.ttf")
(defparameter *sample* (concatenate 'string "Area util: 12,50 m"
                                    (string (code-char 178))        ; superscript two
                                    " " (string (code-char 8212))   ; em dash
                                    " a" (string (code-char 231)) (string (code-char 227)) "o, n"
                                    (string (code-char 227)) "o, " (string (code-char 201)) "ngulo"))

(defun mono-ns ()
  (multiple-value-bind (sec nsec) (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* sec 1000000000) nsec)))

(defparameter *vertex-shader* "#version 330 core
layout(location=0) in vec2 pos;
layout(location=1) in vec2 uv;
uniform vec2 viewport;
out vec2 v_uv;
void main() {
  gl_Position = vec4(pos.x / viewport.x * 2.0 - 1.0, 1.0 - pos.y / viewport.y * 2.0, 0.0, 1.0);
  v_uv = uv;
}")

(defparameter *fragment-shader* "#version 330 core
in vec2 v_uv;
uniform sampler2D atlas;
uniform vec4 color;
out vec4 frag;
void main() { frag = vec4(color.rgb, color.a * texture(atlas, v_uv).r); }")

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
      (error "Link failed: ~a" (gl:get-program-info-log program)))
    program))

(defun drawable-size (window)
  (cffi:with-foreign-objects ((w :int) (h :int))
    (sdl2-ffi.functions:sdl-gl-get-drawable-size window w h)
    (values (cffi:mem-ref w :int) (cffi:mem-ref h :int))))

;;; Glyph atlas: shelf packing into a single-channel texture, rasterized per pixel size.
(defstruct glyph u0 v0 u1 v1 width height left top advance)
(defstruct atlas texture (size 2048) (cursor-x 0) (cursor-y 0) (row-height 0) (table (make-hash-table :test 'equal)))

(defun make-atlas-texture (size)
  (let ((texture (gl:gen-texture)))
    (gl:bind-texture :texture-2d texture)
    (gl:pixel-store :unpack-alignment 1)
    (gl:tex-image-2d :texture-2d 0 :r8 size size 0 :red :unsigned-byte (cffi:null-pointer))
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    texture))

(defun glyph-bitmap-array (face)
  "Copy the current glyph slot bitmap into a (unsigned-byte 8) vector; return vector, width, height."
  (let* ((slot (freetype2-types:ft-face-glyph face))
         (bitmap (freetype2-types:ft-glyphslot-bitmap slot))
         (width (freetype2-types:ft-bitmap-width bitmap))
         (rows (freetype2-types:ft-bitmap-rows bitmap))
         (pitch (freetype2-types:ft-bitmap-pitch bitmap))
         (buffer (freetype2-types:ft-bitmap-buffer bitmap))
         (data (make-array (* width rows) :element-type '(unsigned-byte 8))))
    (dotimes (row rows)
      (dotimes (col width)
        (setf (aref data (+ (* row width) col))
              (cffi:mem-ref buffer :unsigned-char (+ (* row (abs pitch)) col)))))
    (values data width rows)))

(defun ensure-glyph (atlas face pixel-size char)
  "Rasterize CHAR at PIXEL-SIZE into ATLAS (once) and return its glyph record."
  (let ((key (cons pixel-size char)))
    (or (gethash key (atlas-table atlas))
        (progn
          (freetype2:set-pixel-sizes face 0 pixel-size)
          (freetype2:load-char face char '(:render))
          (multiple-value-bind (data width rows) (glyph-bitmap-array face)
            (let* ((slot (freetype2-types:ft-face-glyph face))
                   (size (atlas-size atlas))
                   (advance (/ (freetype2-types:ft-vector-x (freetype2-types:ft-glyphslot-advance slot)) 64.0)))
              (when (> (+ (atlas-cursor-x atlas) width 1) size)
                (setf (atlas-cursor-x atlas) 0
                      (atlas-cursor-y atlas) (+ (atlas-cursor-y atlas) (atlas-row-height atlas) 1)
                      (atlas-row-height atlas) 0))
              (when (> (+ (atlas-cursor-y atlas) rows 1) size)
                (error "Atlas full"))
              (when (plusp (* width rows))
                (gl:bind-texture :texture-2d (atlas-texture atlas))
                (gl:pixel-store :unpack-alignment 1)
                (cffi:with-pointer-to-vector-data (ptr data)
                  (%gl:tex-sub-image-2d :texture-2d 0 (atlas-cursor-x atlas) (atlas-cursor-y atlas)
                                        width rows :red :unsigned-byte ptr)))
              (let ((glyph (make-glyph :u0 (/ (atlas-cursor-x atlas) (float size 1.0))
                                       :v0 (/ (atlas-cursor-y atlas) (float size 1.0))
                                       :u1 (/ (+ (atlas-cursor-x atlas) width) (float size 1.0))
                                       :v1 (/ (+ (atlas-cursor-y atlas) rows) (float size 1.0))
                                       :width width :height rows
                                       :left (freetype2-types:ft-glyphslot-bitmap-left slot)
                                       :top (freetype2-types:ft-glyphslot-bitmap-top slot)
                                       :advance advance)))
                (incf (atlas-cursor-x atlas) (1+ width))
                (setf (atlas-row-height atlas) (max (atlas-row-height atlas) rows))
                (setf (gethash key (atlas-table atlas)) glyph))))))))

(defun draw-text (atlas face program vbo text x baseline pixel-size color
                  &key (scale 1.0) (advance-ratio nil))
  "Draw TEXT with bitmaps rasterized at PIXEL-SIZE, scaled by SCALE. Fixed advance when ADVANCE-RATIO."
  (let ((vertices (make-array (* 6 4 (length text)) :element-type 'single-float :initial-element 0.0))
        (index 0) (pen x))
    (loop for char across text
          for glyph = (ensure-glyph atlas face pixel-size char)
          do (let* ((x0 (+ pen (* scale (glyph-left glyph))))
                    (y0 (- baseline (* scale (glyph-top glyph))))
                    (x1 (+ x0 (* scale (glyph-width glyph))))
                    (y1 (+ y0 (* scale (glyph-height glyph)))))
               (dolist (v (list (list x0 y0 (glyph-u0 glyph) (glyph-v0 glyph))
                                (list x1 y0 (glyph-u1 glyph) (glyph-v0 glyph))
                                (list x1 y1 (glyph-u1 glyph) (glyph-v1 glyph))
                                (list x0 y0 (glyph-u0 glyph) (glyph-v0 glyph))
                                (list x1 y1 (glyph-u1 glyph) (glyph-v1 glyph))
                                (list x0 y1 (glyph-u0 glyph) (glyph-v1 glyph))))
                 (loop for value in v do (setf (aref vertices index) (float value 1.0)) (incf index)))
               (incf pen (if advance-ratio
                             (* advance-ratio pixel-size scale)
                             (* scale (glyph-advance glyph))))))
    (gl:use-program program)
    (gl:uniformf (gl:get-uniform-location program "color") (first color) (second color) (third color) (fourth color))
    (gl:bind-buffer :array-buffer vbo)
    (cffi:with-pointer-to-vector-data (ptr vertices)
      (%gl:buffer-data :array-buffer (* 4 index) ptr :stream-draw))
    (gl:draw-arrays :triangles 0 (floor index 4))
    pen))

(defun run ()
  (sdl2:with-init (:video)
    (sdl2:gl-set-attr :context-major-version 3)
    (sdl2:gl-set-attr :context-minor-version 3)
    (sdl2:gl-set-attr :context-profile-mask sdl2-ffi:+sdl-gl-context-profile-core+)
    (sdl2:with-window (window :title "Borba CAD S3" :w 1280 :h 800 :flags '(:shown :opengl))
      (sdl2:with-gl-context (context window)
        (sdl2:gl-make-current window context)
        (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl2:gl-get-proc-address)
        (freetype2:with-open-face (face *font-path*)
          ;; Metrics: is the font really monospace at 0.6 em?
          (freetype2:set-pixel-sizes face 0 1000)
          (flet ((advance-of (char)
                   (freetype2:load-char face char '(:no-bitmap))
                   (/ (freetype2-types:ft-vector-x
                       (freetype2-types:ft-glyphslot-advance (freetype2-types:ft-face-glyph face)))
                      64.0)))
            (format t "advance at 1000 px: M=~a i=~a W=~a e-acute=~a (ratio ~a)~%"
                    (advance-of #\M) (advance-of #\i) (advance-of #\W) (advance-of (code-char 233))
                    (/ (advance-of #\M) 1000.0)))
          (let* ((atlas (make-atlas :texture (make-atlas-texture 2048)))
                 (program (make-program))
                 (vao (gl:gen-vertex-array))
                 (vbo (gl:gen-buffer)))
            (gl:bind-vertex-array vao)
            (gl:bind-buffer :array-buffer vbo)
            (gl:vertex-attrib-pointer 0 2 :float nil 16 0)
            (gl:enable-vertex-attrib-array 0)
            (gl:vertex-attrib-pointer 1 2 :float nil 16 8)
            (gl:enable-vertex-attrib-array 1)
            (gl:enable :blend)
            (gl:blend-func :src-alpha :one-minus-src-alpha)
            (multiple-value-bind (w h) (drawable-size window)
              (gl:viewport 0 0 w h)
              (gl:clear-color 0.08 0.09 0.11 1.0)
              (gl:clear :color-buffer-bit)
              (gl:use-program program)
              (gl:uniformf (gl:get-uniform-location program "viewport") (float w 1.0) (float h 1.0))
              (gl:uniformi (gl:get-uniform-location program "atlas") 0)
              ;; Rasterization cost: 100 distinct glyphs at one new size.
              (let ((t0 (mono-ns)))
                (loop for code from 33 below 133 do (ensure-glyph atlas face 37 (code-char code)))
                (format t "rasterize+upload 100 glyphs @37px: ~,2f ms~%" (/ (- (mono-ns) t0) 1e6)))
              (let ((white '(0.9 0.9 0.9 1.0)) (cyan '(0.3 0.8 1.0 1.0)) (orange '(1.0 0.7 0.3 1.0)))
                ;; A: bucketed rasterization (a bitmap per size): crisp at every size.
                (draw-text atlas face program vbo "A) rasterized per size:" 20.0 24.0 13 orange)
                (draw-text atlas face program vbo *sample* 20.0 54.0 13 white)
                (draw-text atlas face program vbo *sample* 20.0 110.0 32 white)
                (draw-text atlas face program vbo *sample* 20.0 230.0 96 cyan)
                ;; B: one 48 px bitmap scaled (bilinear) to other sizes: shows blur/pixelation cost.
                (draw-text atlas face program vbo "B) one 48px bitmap scaled:" 20.0 330.0 13 orange)
                (draw-text atlas face program vbo *sample* 20.0 360.0 48 white :scale (/ 13.0 48.0))
                (draw-text atlas face program vbo *sample* 20.0 420.0 48 white :scale (/ 32.0 48.0))
                (draw-text atlas face program vbo *sample* 20.0 550.0 48 cyan :scale 2.0)
                ;; C: fixed advance (0.6 em) layout vs the glyph's own advance: identical for a monospace font.
                (draw-text atlas face program vbo "C) fixed-advance layout (0.6 em):" 20.0 620.0 13 orange)
                (let ((end (draw-text atlas face program vbo *sample* 20.0 680.0 32 white :advance-ratio 0.6)))
                  (format t "fixed-advance width @32px: ~,2f px for ~d chars (expected ~,2f)~%"
                          (- end 20.0) (length *sample*) (* 0.6 32 (length *sample*)))))
              (let ((pixels (gl:read-pixels 0 0 w h :rgb :unsigned-byte)))
                (spike-png:write-rgb-png "/home/borba/Documents/borba-cad/spikes/out/s3.png" w h pixels)))))))))

(run)
(format t "~%S3 finished OK~%")
