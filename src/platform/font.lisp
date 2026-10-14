(in-package #:borba-cad/platform)

;;;; The glyph atlas. FreeType rasterizes each (font, size, code point) once into a shelf-packed
;;;; single-channel texture. Drawing text (the :mono font) is laid out on the fixed 0.6 em grid of
;;;; the geometry module, so only its bitmaps come from the font; the interface fonts (:ui and
;;;; :ui-strong) are proportional and their advances come from the font, through the same cache
;;;; that serves the text measurer, so that layout and drawing always agree. A white block at the
;;;; origin of the texture lets solid shapes use the same shader as glyphs.

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

(defparameter *font-roles* '(:mono :ui :ui-strong)
  "The fonts the program draws with, in the order of their index in the atlas: drawing text, the
interface text and the interface's emphasized text.")

(defparameter *font-candidates*
  '((:mono "/usr/share/fonts/noto/NotoSansMono-Regular.ttf"
     "/usr/share/fonts/TTF/DejaVuSansMono.ttf"
     "/usr/share/fonts/dejavu/DejaVuSansMono.ttf"
     "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
     "/usr/share/fonts/liberation/LiberationMono-Regular.ttf"
     "/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf"
     "/usr/share/fonts/gnu-free/FreeMono.otf")
    (:ui "/usr/share/fonts/noto/NotoSans-Regular.ttf"
     "/usr/share/fonts/TTF/DejaVuSans.ttf"
     "/usr/share/fonts/dejavu/DejaVuSans.ttf"
     "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
     "/usr/share/fonts/liberation/LiberationSans-Regular.ttf"
     "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
     "/usr/share/fonts/gnu-free/FreeSans.otf")
    (:ui-strong "/usr/share/fonts/noto/NotoSans-Medium.ttf"
     "/usr/share/fonts/noto/NotoSans-SemiBold.ttf"
     "/usr/share/fonts/noto/NotoSans-Bold.ttf"
     "/usr/share/fonts/TTF/DejaVuSans-Bold.ttf"
     "/usr/share/fonts/dejavu/DejaVuSans-Bold.ttf"
     "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf"
     "/usr/share/fonts/liberation/LiberationSans-Bold.ttf"
     "/usr/share/fonts/truetype/liberation/LiberationSans-Bold.ttf"))
  "Font files searched, in order, for each font role, after the fonts bundled beside the
executable. The interface fonts fall back to the monospace font when none is installed.")

(defparameter *bundled-font-names*
  '((:mono "NotoSansMono-Regular.ttf")
    (:ui "NotoSans-Regular.ttf")
    (:ui-strong "NotoSans-Medium.ttf"))
  "File names looked for in the fonts directory beside the executable, by role.")

(defun executable-directory ()
  "Return the directory that holds the running executable, or NIL when it is not known."
  (let ((runtime sb-ext:*runtime-pathname*))
    (and runtime
         (uiop:pathname-directory-pathname (uiop:ensure-pathname runtime)))))

(defun bundled-font-path (role)
  "Return the font file of ROLE shipped in the fonts directory beside the executable, or NIL.

ROLE: :mono, :ui or :ui-strong."
  (let ((directory (executable-directory))
        (name (second (assoc role *bundled-font-names*))))
    (when (and directory name)
      (let ((path (merge-pathnames (concatenate 'string "fonts/" name) directory)))
        (and (probe-file path) (namestring path))))))

(defun candidate-font-path (role)
  "Return the first font file that exists for ROLE: the bundled one, then the usual locations;
NIL when there is none.

ROLE: :mono, :ui or :ui-strong."
  (or (bundled-font-path role)
      (find-if #'probe-file (rest (assoc role *font-candidates*)))))

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
          (t (or (candidate-font-path :mono)
                 (error 'base:configuration-error
                        :reason (format nil "no monospace font was found; install one (for example ~
                                             Noto Sans Mono) or pass --font FILE or set BORBA_CAD_FONT")))))))

(defun find-font-paths (config)
  "Return the font file of every role as a property list (:mono path :ui path :ui-strong path).
The interface fonts fall back to the monospace font, and the strong one to the regular one, so
text is always drawn, only less handsomely. Signals CONFIGURATION-ERROR when no monospace font
exists.

CONFIG: the application configuration."
  (let* ((mono (find-font-path config))
         (regular (or (candidate-font-path :ui) mono))
         (strong (or (candidate-font-path :ui-strong) regular)))
    (list :mono mono
          :ui regular
          :ui-strong strong)))

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

(defconstant +advance-reference-size+ 1000
  "Pixel size at which glyph advances are read, large enough that hinting does not matter.")

(defstruct (font-set (:constructor %make-font-set (library
                                                   faces)))
  "The open fonts of the program, with the advance of every glyph read so far.

LIBRARY: the FreeType library handle that owns every face.
FACES: simple vector of faces, in the order of *FONT-ROLES*.
ADVANCES: per font, a table from code point to horizontal advance in em."
  library
  faces
  (advances (coerce (loop repeat (length *font-roles*) collect (make-hash-table)) 'simple-vector)))

(defstruct (glyph-atlas (:constructor %make-glyph-atlas (fonts
                                                         texture)))
  "The glyph texture and its bookkeeping.

FONTS: the font set the glyphs come from.
TEXTURE: the OpenGL texture name.
CURSOR-X: next free x on the current shelf.
CURSOR-Y: top of the current shelf.
ROW-HEIGHT: height of the tallest glyph on the current shelf.
TABLE: glyph key to GLYPH (or :empty for glyphs without a bitmap).
FULL: true when a glyph did not fit; the atlas is cleared before the next frame."
  fonts texture
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

(defun font-index (role)
  "Return the position of a font role in the font set; unknown roles use the interface font.

ROLE: :mono, :ui or :ui-strong."
  (or (position role *font-roles*)
      (position :ui *font-roles*)))

;;; cl-freetype2 frees faces and libraries from finalizers in no particular order, and freeing a
;;; face after its library is undefined behavior. The handles are therefore kept alive for the
;;; life of the process (a few font files and a few kilobytes).
(sb-ext:defglobal **retained-freetype-handles** nil)

(defun open-font-set (paths)
  "Open the fonts of every role. It needs no OpenGL context, so the text measurer can exist
before the window. Every font set creates its own FreeType library handle because the one
cl-freetype2 makes when it loads is invalid in a saved executable.

PATHS: property list (:mono path :ui path :ui-strong path) of font files."
  (let* ((library (freetype2:make-freetype))
         (faces (coerce (loop for role in *font-roles*
                              collect (freetype2:new-face (getf paths role) 0 library))
                        'simple-vector)))
    (push (cons library faces) **retained-freetype-handles**)
    (%make-font-set library faces)))

(defun glyph-advance-em (fonts
                         role
                         code)
  "Return the horizontal advance of a code point in em, a fraction of the font size. It is read
from the font without hinting, once, and cached.

FONTS: the font set.
ROLE: :mono, :ui or :ui-strong.
CODE: Unicode code point."
  (let* ((index (font-index role))
         (table (svref (font-set-advances fonts) index)))
    (or (gethash code table)
        (setf (gethash code table)
              (let ((face (svref (font-set-faces fonts) index)))
                (freetype2:set-pixel-sizes face 0 +advance-reference-size+)
                (freetype2:load-char face (code-char code) '(:no-hinting :no-bitmap))
                (/ (float (freetype2:get-loaded-advance face nil) 1f0)
                   (float +advance-reference-size+ 1f0)))))))

(defun font-set-text-width (fonts
                            role
                            string
                            size)
  "Return the width of STRING at SIZE in a font. Drawing text (:mono) sits on the fixed
0.6 em grid of the geometry module; the interface fonts use the advances of their glyphs.

FONTS: the font set.
ROLE: :mono, :ui or :ui-strong.
STRING: the text.
SIZE: em size in pixels."
  (if (eq role :mono)
      (ui:fixed-advance-width role
                              string
                              size)
      (* size (loop for character across string
                    sum (glyph-advance-em fonts
                                          role
                                          (char-code character))))))

(defun font-set-measurer (fonts)
  "Return a text measurer for the interface (see UI:*TEXT-MEASURER*) that reads the glyph
advances of FONTS, so that layout and drawing agree to the pixel.

FONTS: the font set."
  (lambda (role string size)
    (font-set-text-width fonts
                         role
                         string
                         size)))

(defun make-glyph-atlas (fonts)
  "Create the atlas: the empty texture the glyphs of FONTS are rasterized into. Needs a current
OpenGL context.

FONTS: the font set."
  (let ((texture (gl:gen-texture)))
    (gl:bind-texture :texture-2d texture)
    (gl:pixel-store :unpack-alignment 1)
    (gl:tex-image-2d :texture-2d 0 :r8 +atlas-size+ +atlas-size+ 0 :red :unsigned-byte (cffi:null-pointer))
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (upload-white-block)
    (%make-glyph-atlas fonts texture)))

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
  "Release the texture; the font handles stay alive (see OPEN-FONT-SET).

ATLAS: the glyph atlas."
  (gl:delete-textures (list (glyph-atlas-texture atlas)))
  atlas)

(defun glyph-key (font-index
                  raster-size
                  code)
  "Return the table key of a (font, size, code point) triple.

FONT-INDEX: position of the font in the font set.
RASTER-SIZE: rasterized pixel size.
CODE: Unicode code point."
  (logior (ash font-index 40) (ash raster-size 21) code))

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
                        font-index
                        raster-size
                        code)
  "Rasterize one glyph into the atlas and return its record, :EMPTY for a glyph without pixels,
or NIL when the atlas is full.

ATLAS: the glyph atlas.
FONT-INDEX: position of the font in the font set.
RASTER-SIZE: pixel size.
CODE: Unicode code point."
  (let ((face (svref (font-set-faces (glyph-atlas-fonts atlas)) font-index)))
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
                     font-index
                     raster-size
                     code)
  "Return the glyph record for a (font, size, code point) triple, rasterizing it on first use;
:EMPTY for glyphs without pixels (spaces), NIL when the atlas is full.

ATLAS: the glyph atlas.
FONT-INDEX: position of the font in the font set.
RASTER-SIZE: pixel size.
CODE: Unicode code point."
  (let ((key (glyph-key font-index
                        raster-size
                        code)))
    (or (gethash key (glyph-atlas-table atlas))
        (let ((glyph (rasterize-glyph atlas
                                      font-index
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
Returns the (possibly reallocated) buffer and the next free index. Interface text, which does
not rotate, has every glyph placed on a whole framebuffer pixel so that it stays crisp.

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
         (role (scene:text-run-font run))
         (font-index (font-index role))
         (fonts (glyph-atlas-fonts atlas))
         (proportional (not (eq role :mono)))
         (raster (glyph-raster-size (* size scale)))
         (factor (/ size (float raster 1f0)))
         (fixed-advance (* size (float geo:+text-advance-ratio+ 1f0)))
         (cosine (cos angle))
         (sine (sin angle))
         ;; Baseline direction and the direction of "up", in screen coordinates (y down).
         (along-x cosine) (along-y (- sine))
         (up-x (- sine)) (up-y (- cosine))
         (origin-x (if (zerop angle) (/ (fround (* (scene:text-run-x run) scale)) scale) (scene:text-run-x run)))
         (origin-y (if (zerop angle) (/ (fround (* (scene:text-run-y run) scale)) scale) (scene:text-run-y run)))
         (index start)
         (pen 0f0)
         (output (ensure-text-capacity buffer (+ start (* +floats-per-glyph+ (length string))))))
    (declare (type single-float scale factor fixed-advance cosine sine along-x along-y up-x up-y origin-x
                   origin-y pen)
             (type fixnum index))
    (loop for character across string
          do (let* ((code (char-code character))
                    (glyph (ensure-glyph atlas
                                         font-index
                                         raster
                                         code))
                    (placed-pen (if (and proportional (zerop angle))
                                    (- (/ (fround (* (+ origin-x pen) scale)) scale) origin-x)
                                    pen)))
               (declare (type single-float placed-pen))
               (when (and glyph (not (eq glyph :empty)))
                 (let* ((left (* factor (glyph-left glyph)))
                        (right (+ left (* factor (glyph-width glyph))))
                        (top (* factor (glyph-top glyph)))
                        (bottom (- top (* factor (glyph-height glyph)))))
                   (flet ((corner (along-distance up-distance u v)
                            (let ((x (+ origin-x (* along-x (+ placed-pen along-distance)) (* up-x up-distance)))
                                  (y (+ origin-y (* along-y (+ placed-pen along-distance)) (* up-y up-distance))))
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
                       (corner left bottom u0 v1)))))
               (incf pen (if proportional
                             (* size (the single-float (glyph-advance-em fonts
                                                                         role
                                                                         code)))
                             fixed-advance))))
    (values output index)))
