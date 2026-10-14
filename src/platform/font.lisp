(in-package #:borba-cad/platform)

;;;; The glyph atlas. FreeType rasterizes each (size, code point) once into a shelf-packed
;;;; single-channel texture. Text is laid out on the fixed 0.6 em grid of the geometry module, so
;;;; only the bitmaps come from the font, never the advances. A white block at the origin of the
;;;; texture lets solid shapes use the same shader as glyphs.

(defconstant +atlas-size+ 2048
  "Width and height of the glyph atlas texture, in texels.")

(defconstant +white-block-size+ 4
  "Side of the opaque white block reserved at the origin of the atlas.")

(defconstant +glyph-padding+ 1
  "Empty texels around every glyph, so bilinear filtering never samples a neighbor.")

(defconstant +minimum-raster-size+ 4
  "Smallest pixel size glyphs are rasterized at.")

(defconstant +exact-raster-limit+ 32
  "Up to this pixel size every integer size is rasterized exactly.")

(defconstant +maximum-raster-size+ 256
  "Largest pixel size glyphs are rasterized at; bigger text scales these bitmaps up.")

(defconstant +raster-ladder-ratio+ 1.25d0
  "Ratio between consecutive rasterized sizes above the exact limit.")

(defparameter *font-candidates*
  '("/usr/share/fonts/noto/NotoSansMono-Regular.ttf"
    "/usr/share/fonts/TTF/DejaVuSansMono.ttf"
    "/usr/share/fonts/dejavu/DejaVuSansMono.ttf"
    "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
    "/usr/share/fonts/liberation/LiberationMono-Regular.ttf"
    "/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf"
    "/usr/share/fonts/gnu-free/FreeMono.otf")
  "Monospace fonts searched, in order, when none is configured.")

(defun find-font-path (config)
  "Return the path of the monospace font to use: the configured one, or the first usual
location that exists. Signals CONFIGURATION-ERROR when there is none.

CONFIG: the application configuration."
  (let ((configured (base:config-font-path config)))
    (cond (configured
           (unless (probe-file configured)
             (error 'base:configuration-error
                    :reason (format nil "the font file ~a does not exist" configured)))
           configured)
          (t (or (find-if #'probe-file *font-candidates*)
                 (error 'base:configuration-error
                        :reason (format nil "no monospace font was found; install one (for example ~
                                             Noto Sans Mono) or pass --font FILE or set BORBA_CAD_FONT")))))))

(defun glyph-raster-size (physical-size)
  "Return the pixel size at which glyphs for text of PHYSICAL-SIZE pixels are rasterized:
exact integers up to 32, then a ladder of sizes growing by 25 percent, capped at 256.

PHYSICAL-SIZE: the on-screen em size in framebuffer pixels."
  (let ((rounded (max +minimum-raster-size+ (round physical-size))))
    (if (<= rounded +exact-raster-limit+)
        rounded
        (let ((size (float +exact-raster-limit+ 1d0)))
          (loop while (< (round size) rounded)
                do (setf size (* size +raster-ladder-ratio+)))
          (min +maximum-raster-size+ (round size))))))

(defstruct (glyph (:constructor make-glyph (u0
                                            v0
                                            u1
                                            v1
                                            width
                                            height
                                            left
                                            top)))
  "Where one rasterized glyph lives in the atlas and how it sits on the baseline.

U0: left texture coordinate.
V0: top texture coordinate.
U1: right texture coordinate.
V1: bottom texture coordinate.
WIDTH: bitmap width in texels.
HEIGHT: bitmap height in texels.
LEFT: pen-to-bitmap offset in texels.
TOP: baseline-to-bitmap-top offset in texels."
  (u0 0f0 :type single-float) (v0 0f0 :type single-float)
  (u1 0f0 :type single-float) (v1 0f0 :type single-float)
  (width 0 :type fixnum) (height 0 :type fixnum)
  (left 0 :type fixnum) (top 0 :type fixnum))

(defstruct (glyph-atlas (:constructor %make-glyph-atlas (library
                                                         face
                                                         texture)))
  "The glyph texture and its bookkeeping.

LIBRARY: the FreeType library handle owned by this atlas.
FACE: the open font face.
TEXTURE: the OpenGL texture name.
CURSOR-X: next free x on the current shelf.
CURSOR-Y: top of the current shelf.
ROW-HEIGHT: height of the tallest glyph on the current shelf.
TABLE: glyph key to GLYPH (or :empty for glyphs without a bitmap).
FULL: true when a glyph did not fit; the atlas is cleared before the next frame."
  library face texture
  (cursor-x +white-block-size+ :type fixnum)
  (cursor-y 0 :type fixnum)
  (row-height 0 :type fixnum)
  (table (make-hash-table) :type hash-table)
  (full nil))

(defun upload-white-block ()
  "Fill the reserved block at the origin of the bound atlas texture with opaque white."
  (let ((block (make-array (* +white-block-size+ +white-block-size+)
                           :element-type '(unsigned-byte 8) :initial-element 255)))
    (cffi:with-pointer-to-vector-data (pointer block)
      (%gl:tex-sub-image-2d :texture-2d 0 0 0 +white-block-size+ +white-block-size+
                            :red :unsigned-byte pointer))))

;;; cl-freetype2 frees faces and libraries from finalizers in no particular order, and freeing a
;;; face after its library is undefined behavior. The handles are therefore kept alive for the
;;; life of the process (a font file and a few kilobytes per atlas).
(sb-ext:defglobal **retained-freetype-handles** nil)

(defun make-glyph-atlas (font-path)
  "Create the atlas: open the font and the empty texture. Needs a current OpenGL context.
Every atlas creates its own FreeType library handle because the one cl-freetype2 makes when it
loads is invalid in a saved executable.

FONT-PATH: path of a TrueType or OpenType font file."
  (let* ((library (freetype2:make-freetype))
         (face (freetype2:new-face font-path 0 library))
         (texture (gl:gen-texture)))
    (push (cons library face) **retained-freetype-handles**)
    (gl:bind-texture :texture-2d texture)
    (gl:pixel-store :unpack-alignment 1)
    (gl:tex-image-2d :texture-2d 0 :r8 +atlas-size+ +atlas-size+ 0 :red :unsigned-byte (cffi:null-pointer))
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (upload-white-block)
    (%make-glyph-atlas library
                       face
                       texture)))

(defun atlas-white-uv ()
  "Return the texture coordinate of the middle of the white block."
  (/ (* 0.5f0 +white-block-size+) +atlas-size+))

(defun reset-glyph-atlas (atlas)
  "Forget every glyph so the atlas can be refilled; the texture itself is overwritten lazily.

ATLAS: the glyph atlas."
  (clrhash (glyph-atlas-table atlas))
  (setf (glyph-atlas-cursor-x atlas) +white-block-size+
        (glyph-atlas-cursor-y atlas) 0
        (glyph-atlas-row-height atlas) 0
        (glyph-atlas-full atlas) nil))

(defun close-glyph-atlas (atlas)
  "Release the texture; the font handles stay alive (see MAKE-GLYPH-ATLAS).

ATLAS: the glyph atlas."
  (gl:delete-textures (list (glyph-atlas-texture atlas)))
  atlas)

(defun glyph-key (raster-size
                  code)
  "Return the table key of a (size, code point) pair.

RASTER-SIZE: rasterized pixel size.
CODE: Unicode code point."
  (logior (ash raster-size 21) code))

(defun copy-glyph-bitmap (face)
  "Copy the bitmap of the glyph currently loaded in FACE; returns the octets, the width and the
height.

FACE: a FreeType face with a rendered glyph."
  (let* ((slot (freetype2-types:ft-face-glyph face))
         (bitmap (freetype2-types:ft-glyphslot-bitmap slot))
         (width (freetype2-types:ft-bitmap-width bitmap))
         (rows (freetype2-types:ft-bitmap-rows bitmap))
         (pitch (abs (freetype2-types:ft-bitmap-pitch bitmap)))
         (buffer (freetype2-types:ft-bitmap-buffer bitmap))
         (data (make-array (* width rows) :element-type '(unsigned-byte 8))))
    (dotimes (row rows)
      (dotimes (column width)
        (setf (aref data (+ (* row width) column))
              (cffi:mem-ref buffer :unsigned-char (+ (* row pitch) column)))))
    (values data width rows)))

(defun rasterize-glyph (atlas
                        raster-size
                        code)
  "Rasterize one glyph into the atlas and return its record, :EMPTY for a glyph without pixels,
or NIL when the atlas is full.

ATLAS: the glyph atlas.
RASTER-SIZE: pixel size.
CODE: Unicode code point."
  (let ((face (glyph-atlas-face atlas)))
    (freetype2:set-pixel-sizes face 0 raster-size)
    (freetype2:load-char face (code-char code) '(:render))
    (multiple-value-bind (data width rows) (copy-glyph-bitmap face)
      (let ((slot (freetype2-types:ft-face-glyph face)))
        (if (zerop (* width rows))
            :empty
            (progn
              (when (> (+ (glyph-atlas-cursor-x atlas) width +glyph-padding+) +atlas-size+)
                (setf (glyph-atlas-cursor-x atlas) 0
                      (glyph-atlas-cursor-y atlas) (+ (glyph-atlas-cursor-y atlas)
                                                      (glyph-atlas-row-height atlas)
                                                      +glyph-padding+)
                      (glyph-atlas-row-height atlas) 0))
              (if (> (+ (glyph-atlas-cursor-y atlas) rows +glyph-padding+) +atlas-size+)
                  (progn (setf (glyph-atlas-full atlas) t) nil)
                  (let ((x (glyph-atlas-cursor-x atlas))
                        (y (glyph-atlas-cursor-y atlas)))
                    (gl:bind-texture :texture-2d (glyph-atlas-texture atlas))
                    (gl:pixel-store :unpack-alignment 1)
                    (cffi:with-pointer-to-vector-data (pointer data)
                      (%gl:tex-sub-image-2d :texture-2d 0 x y width rows :red :unsigned-byte pointer))
                    (incf (glyph-atlas-cursor-x atlas) (+ width +glyph-padding+))
                    (setf (glyph-atlas-row-height atlas) (max (glyph-atlas-row-height atlas) rows))
                    (make-glyph (/ (float x 1f0) +atlas-size+)
                                (/ (float y 1f0) +atlas-size+)
                                (/ (float (+ x width) 1f0) +atlas-size+)
                                (/ (float (+ y rows) 1f0) +atlas-size+)
                                width
                                rows
                                (freetype2-types:ft-glyphslot-bitmap-left slot)
                                (freetype2-types:ft-glyphslot-bitmap-top slot))))))))))

(defun ensure-glyph (atlas
                     raster-size
                     code)
  "Return the glyph record for a (size, code point) pair, rasterizing it on first use; :EMPTY
for glyphs without pixels (spaces), NIL when the atlas is full.

ATLAS: the glyph atlas.
RASTER-SIZE: pixel size.
CODE: Unicode code point."
  (let ((key (glyph-key raster-size code)))
    (or (gethash key (glyph-atlas-table atlas))
        (let ((glyph (rasterize-glyph atlas
                                      raster-size
                                      code)))
          (when glyph
            (setf (gethash key (glyph-atlas-table atlas)) glyph))
          glyph))))

(defconstant +floats-per-glyph+ (* 6 scene:+floats-per-vertex+)
  "Floats emitted per glyph: two triangles of eight-float vertices.")

(defun ensure-text-capacity (buffer
                             needed)
  "Return BUFFER, or a larger array when NEEDED floats do not fit.

BUFFER: a simple single-float array.
NEEDED: required length."
  (if (>= (length buffer) needed)
      buffer
      (make-array (max needed (* 2 (length buffer))) :element-type 'single-float)))

(defun layout-text-run (atlas
                        run
                        scale
                        buffer
                        start)
  "Append the glyph triangles of one text run to BUFFER starting at float index START.
Returns the (possibly reallocated) buffer and the next free index.

ATLAS: the glyph atlas.
RUN: a text run in logical pixels.
SCALE: display scale, framebuffer pixels per logical pixel.
BUFFER: the destination single-float array.
START: index of the first float to write."
  (let* ((scale (float scale 1f0))
         (string (scene:text-run-string run))
         (size (scene:text-run-size run))
         (angle (scene:text-run-angle run))
         (color (scene:text-run-color run))
         (raster (glyph-raster-size (* size scale)))
         (factor (/ size (float raster 1f0)))
         (advance (* size (float geo:+text-advance-ratio+ 1f0)))
         (cosine (cos angle))
         (sine (sin angle))
         ;; Baseline direction and the direction of "up", in screen coordinates (y down).
         (along-x cosine) (along-y (- sine))
         (up-x (- sine)) (up-y (- cosine))
         (origin-x (if (zerop angle) (/ (fround (* (scene:text-run-x run) scale)) scale) (scene:text-run-x run)))
         (origin-y (if (zerop angle) (/ (fround (* (scene:text-run-y run) scale)) scale) (scene:text-run-y run)))
         (index start)
         (output (ensure-text-capacity buffer (+ start (* +floats-per-glyph+ (length string))))))
    (declare (type single-float scale factor advance cosine sine along-x along-y up-x up-y origin-x origin-y)
             (type fixnum index))
    (loop for character across string
          for pen of-type single-float from 0f0 by advance
          do (let ((glyph (ensure-glyph atlas
                                        raster
                                        (char-code character))))
               (when (and glyph (not (eq glyph :empty)))
                 (let* ((left (* factor (glyph-left glyph)))
                        (right (+ left (* factor (glyph-width glyph))))
                        (top (* factor (glyph-top glyph)))
                        (bottom (- top (* factor (glyph-height glyph)))))
                   (flet ((corner (along-distance up-distance u v)
                            (let ((x (+ origin-x (* along-x (+ pen along-distance)) (* up-x up-distance)))
                                  (y (+ origin-y (* along-y (+ pen along-distance)) (* up-y up-distance))))
                              (setf (aref output index) x
                                    (aref output (+ index 1)) y
                                    (aref output (+ index 2)) u
                                    (aref output (+ index 3)) v
                                    (aref output (+ index 4)) (scene:rgba-r color)
                                    (aref output (+ index 5)) (scene:rgba-g color)
                                    (aref output (+ index 6)) (scene:rgba-b color)
                                    (aref output (+ index 7)) (scene:rgba-a color))
                              (incf index scene:+floats-per-vertex+))))
                     (let ((u0 (glyph-u0 glyph)) (v0 (glyph-v0 glyph)) (u1 (glyph-u1 glyph)) (v1 (glyph-v1 glyph)))
                       (corner left top u0 v0)
                       (corner right top u1 v0)
                       (corner right bottom u1 v1)
                       (corner left top u0 v0)
                       (corner right bottom u1 v1)
                       (corner left bottom u0 v1)))))))
    (values output index)))
