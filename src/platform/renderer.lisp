(in-package #:borba-cad/platform)

;;;; The OpenGL renderer of the draw list: one shader, one vertex layout (position, texture
;;;; coordinate, color), lines and triangles replayed from the draw list's vertex array, and
;;;; glyph triangles generated per text command.

(defparameter *vertex-shader-source*
  "#version 330 core
layout(location = 0) in vec2 position;
layout(location = 1) in vec2 texture_coordinate;
layout(location = 2) in vec4 color;
uniform vec2 viewport;
out vec2 v_texture_coordinate;
out vec4 v_color;
void main() {
  gl_Position = vec4(position.x / viewport.x * 2.0 - 1.0, 1.0 - position.y / viewport.y * 2.0, 0.0, 1.0);
  v_texture_coordinate = texture_coordinate;
  v_color = color;
}"
  "GLSL vertex shader: pixel coordinates to clip space.")

(defparameter *fragment-shader-source*
  "#version 330 core
in vec2 v_texture_coordinate;
in vec4 v_color;
uniform sampler2D atlas;
out vec4 fragment;
void main() {
  fragment = vec4(v_color.rgb, v_color.a * texture(atlas, v_texture_coordinate).r);
}"
  "GLSL fragment shader: color times the atlas coverage (1 for solid shapes).")

(defconstant +vertex-stride-bytes+ (* 4 scene:+floats-per-vertex+)
  "Bytes per vertex in the draw list's layout.")

(defconstant +multisample-count+ 4
  "Samples per pixel requested for anti-aliased lines.")

(defstruct (renderer (:constructor %make-renderer (program
                                                   viewport-location
                                                   atlas-location
                                                   atlas
                                                   main-vao
                                                   main-vbo
                                                   text-vao
                                                   text-vbo)))
  "OpenGL objects owned by the renderer.

PROGRAM: the linked shader program.
VIEWPORT-LOCATION: uniform location of the logical window size.
ATLAS-LOCATION: uniform location of the glyph texture sampler.
ATLAS: the glyph atlas.
MAIN-VAO: vertex array for the draw list's lines and triangles.
MAIN-VBO: its buffer.
TEXT-VAO: vertex array for generated glyph triangles.
TEXT-VBO: its buffer.
TEXT-BUFFER: scratch array the glyph triangles are built in."
  program viewport-location atlas-location atlas main-vao main-vbo text-vao text-vbo
  (text-buffer (make-array 8192 :element-type 'single-float) :type (simple-array single-float (*))))

(defun compile-shader (type
                       source)
  "Compile one shader, signaling an error with the driver's log on failure.

TYPE: :vertex-shader or :fragment-shader.
SOURCE: GLSL source text."
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    (unless (gl:get-shader shader :compile-status)
      (error "The ~(~a~) failed to compile: ~a" type (gl:get-shader-info-log shader)))
    shader))

(defun link-program ()
  "Compile and link the renderer's shader program."
  (let ((program (gl:create-program)))
    (gl:attach-shader program (compile-shader :vertex-shader *vertex-shader-source*))
    (gl:attach-shader program (compile-shader :fragment-shader *fragment-shader-source*))
    (gl:link-program program)
    (unless (gl:get-program program :link-status)
      (error "The shader program failed to link: ~a" (gl:get-program-info-log program)))
    program))

(defun configure-vertex-layout (vao
                                vbo)
  "Describe the vertex layout (position, texture coordinate, color) for a vertex array.

VAO: vertex array name.
VBO: buffer name."
  (gl:bind-vertex-array vao)
  (gl:bind-buffer :array-buffer vbo)
  (gl:vertex-attrib-pointer 0 2 :float nil +vertex-stride-bytes+ 0)
  (gl:enable-vertex-attrib-array 0)
  (gl:vertex-attrib-pointer 1 2 :float nil +vertex-stride-bytes+ 8)
  (gl:enable-vertex-attrib-array 1)
  (gl:vertex-attrib-pointer 2 4 :float nil +vertex-stride-bytes+ 16)
  (gl:enable-vertex-attrib-array 2))

(defun make-renderer (font-path)
  "Create the renderer: shader, buffers and glyph atlas. Needs a current OpenGL context.

FONT-PATH: path of the monospace font."
  (let* ((program (link-program))
         (main-vao (gl:gen-vertex-array))
         (main-vbo (gl:gen-buffer))
         (text-vao (gl:gen-vertex-array))
         (text-vbo (gl:gen-buffer)))
    (configure-vertex-layout main-vao main-vbo)
    (configure-vertex-layout text-vao text-vbo)
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (%make-renderer program
                    (gl:get-uniform-location program "viewport")
                    (gl:get-uniform-location program "atlas")
                    (make-glyph-atlas font-path)
                    main-vao
                    main-vbo
                    text-vao
                    text-vbo)))

(defun close-renderer (renderer)
  "Release every OpenGL object and the font.

RENDERER: the renderer."
  (close-glyph-atlas (renderer-atlas renderer))
  (gl:delete-buffers (list (renderer-main-vbo renderer) (renderer-text-vbo renderer)))
  (gl:delete-vertex-arrays (list (renderer-main-vao renderer) (renderer-text-vao renderer)))
  (gl:delete-program (renderer-program renderer))
  renderer)

(defun upload-and-draw (vertices
                        float-count
                        mode
                        first-vertex
                        vertex-count)
  "Upload VERTICES to the bound array buffer and draw part of it.

VERTICES: simple single-float array.
FLOAT-COUNT: floats to upload.
MODE: :lines or :triangles.
FIRST-VERTEX: first vertex to draw.
VERTEX-COUNT: vertices to draw."
  (cffi:with-pointer-to-vector-data (pointer vertices)
    (%gl:buffer-data :array-buffer (* 4 float-count) pointer :stream-draw))
  (gl:draw-arrays mode first-vertex vertex-count))

(defun draw-text-commands (renderer
                           runs
                           start
                           count
                           scale)
  "Generate and draw the glyph triangles of COUNT text runs.

RENDERER: the renderer.
RUNS: the draw list's text runs.
START: index of the first run.
COUNT: number of runs.
SCALE: display scale."
  (let ((buffer (renderer-text-buffer renderer))
        (index 0))
    (loop for run-index from start below (+ start count)
          do (multiple-value-setq (buffer index)
               (layout-text-run (renderer-atlas renderer)
                                (aref runs run-index)
                                scale
                                buffer
                                index)))
    (setf (renderer-text-buffer renderer) buffer)
    (when (plusp index)
      (gl:bind-vertex-array (renderer-text-vao renderer))
      (gl:bind-buffer :array-buffer (renderer-text-vbo renderer))
      (upload-and-draw buffer
                       index
                       :triangles
                       0
                       (floor index scene:+floats-per-vertex+))
      (gl:bind-vertex-array (renderer-main-vao renderer))
      (gl:bind-buffer :array-buffer (renderer-main-vbo renderer)))))

(defun render-draw-list (renderer
                         list
                         logical-width
                         logical-height
                         framebuffer-width
                         framebuffer-height
                         scale
                         background)
  "Draw a whole frame: clear, then replay the draw list in painter's order.

RENDERER: the renderer.
LIST: the draw list.
LOGICAL-WIDTH: window width in logical pixels.
LOGICAL-HEIGHT: window height in logical pixels.
FRAMEBUFFER-WIDTH: drawable width in physical pixels.
FRAMEBUFFER-HEIGHT: drawable height in physical pixels.
SCALE: framebuffer pixels per logical pixel.
BACKGROUND: an RGBA clear color."
  (when (glyph-atlas-full (renderer-atlas renderer))
    (reset-glyph-atlas (renderer-atlas renderer)))
  (gl:viewport 0 0 framebuffer-width framebuffer-height)
  (gl:clear-color (scene:rgba-r background) (scene:rgba-g background) (scene:rgba-b background) 1f0)
  (gl:clear :color-buffer-bit)
  (gl:use-program (renderer-program renderer))
  (gl:uniformf (renderer-viewport-location renderer) (float logical-width 1f0) (float logical-height 1f0))
  (gl:uniformi (renderer-atlas-location renderer) 0)
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (glyph-atlas-texture (renderer-atlas renderer)))
  (gl:bind-vertex-array (renderer-main-vao renderer))
  (gl:bind-buffer :array-buffer (renderer-main-vbo renderer))
  (let ((float-count (* scene:+floats-per-vertex+ (scene:draw-list-vertex-count list))))
    (cffi:with-pointer-to-vector-data (pointer (scene:draw-list-vertices list))
      (%gl:buffer-data :array-buffer (* 4 float-count) pointer :stream-draw)))
  (let ((runs (scene:draw-list-text-runs list)))
    (loop for command across (scene:draw-list-commands list)
          do (ecase (scene:draw-command-kind command)
               (:lines (gl:draw-arrays :lines
                                       (scene:draw-command-start command)
                                       (scene:draw-command-count command)))
               (:triangles (gl:draw-arrays :triangles
                                           (scene:draw-command-start command)
                                           (scene:draw-command-count command)))
               (:text (draw-text-commands renderer
                                          runs
                                          (scene:draw-command-start command)
                                          (scene:draw-command-count command)
                                          scale))))))
