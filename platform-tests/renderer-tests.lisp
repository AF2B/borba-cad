(in-package #:borba-cad/platform-tests)

;;;; These tests open one real window, render known scenes through the production renderer,
;;;; read the pixels back and judge them. All observations are taken inside a single window
;;;; session (SDL runs its body in another thread, where FiveAM's bookkeeping is not available)
;;;; and the assertions are made afterwards in the test thread.

(def-suite renderer
  :description "The OpenGL renderer, the glyph atlas and a whole application frame in a real window."
  :in all)

(in-suite renderer)

(defconstant +test-window-width+ 1280
  "Logical width of the test window; larger than the program's minimum window size.")

(defconstant +test-window-height+ 720
  "Logical height of the test window; larger than the program's minimum window size.")

(defconstant +channel-tolerance+ 4
  "Largest difference, per 8-bit channel, between a rendered and an expected flat color.")

(defconstant +ink-threshold+ 60
  "Sum of channel differences from the background above which a pixel counts as drawn.")

(defparameter *test-background* (scene:make-rgba 0.10f0 0.20f0 0.30f0 1f0)
  "Clear color of the test scenes: dark blue, far from every drawing color used.")

(defparameter *observations* nil
  "Property list of everything observed in the shared window session, or NIL before the first
renderer test runs.")

(defstruct (frame (:constructor make-frame (pixels
                                            width
                                            height
                                            scale)))
  "A rendered frame read back from the framebuffer.

PIXELS: RGB octets, bottom row first.
WIDTH: framebuffer width in pixels.
HEIGHT: framebuffer height in pixels.
SCALE: framebuffer pixels per logical pixel."
  pixels width height scale)

(defun frame-pixel (frame
                    x
                    y)
  "Return the (r g b) octets at a logical position of a frame (y counted from the top).

FRAME: the frame.
X: logical x.
Y: logical y."
  (let* ((column (min (1- (frame-width frame)) (round (* (frame-scale frame) x))))
         (row (- (frame-height frame) 1 (min (1- (frame-height frame)) (round (* (frame-scale frame) y)))))
         (base (* 3 (+ column (* row (frame-width frame))))))
    (list (aref (frame-pixels frame) base)
          (aref (frame-pixels frame) (+ base 1))
          (aref (frame-pixels frame) (+ base 2)))))

(defun color-octets (color)
  "Return an RGBA color as a list of three 8-bit channel values.

COLOR: an RGBA."
  (flet ((octet (channel) (round (* 255 channel))))
    (list (octet (scene:rgba-r color)) (octet (scene:rgba-g color)) (octet (scene:rgba-b color)))))

(defun same-color-p (actual
                     expected)
  "Return true when two (r g b) lists agree within the channel tolerance.

ACTUAL: the rendered color.
EXPECTED: the wanted color."
  (every (lambda (a e) (<= (abs (- a e)) +channel-tolerance+)) actual expected))

(defun count-ink (frame
                  left
                  top
                  right
                  bottom
                  background)
  "Count the pixels of a logical rectangle that differ clearly from the background.

FRAME: the frame.
LEFT: first logical x.
TOP: first logical y.
RIGHT: logical x just past the last.
BOTTOM: logical y just past the last.
BACKGROUND: the (r g b) background."
  (let ((count 0))
    (loop for y from top below bottom
          do (loop for x from left below right
                   do (when (> (reduce #'+ (mapcar (lambda (a b) (abs (- a b)))
                                                   (frame-pixel frame x y)
                                                   background))
                               +ink-threshold+)
                        (incf count))))
    count))

(defun render-frame (window
                     renderer
                     list
                     &key (background *test-background*))
  "Render LIST through the production renderer and read the back buffer.

WINDOW: the SDL window.
RENDERER: the renderer.
LIST: the draw list.
BACKGROUND: the RGBA clear color."
  (setf (scene:draw-list-white-u list) (platform::atlas-white-uv)
        (scene:draw-list-white-v list) (platform::atlas-white-uv))
  (multiple-value-bind (logical-width logical-height scale framebuffer-width framebuffer-height)
      (platform::present-frame window renderer list background)
    (declare (ignore logical-width logical-height))
    (make-frame (gl:read-pixels 0 0 framebuffer-width framebuffer-height :rgb :unsigned-byte)
                framebuffer-width
                framebuffer-height
                scale)))

(defun observe-shapes (window
                       renderer)
  "Render a rectangle, a triangle and a line, and report what the pixels show.

WINDOW: the SDL window.
RENDERER: the renderer."
  (let ((list (scene:make-draw-list))
        (red (scene:make-rgba 1f0 0f0 0f0 1f0))
        (green (scene:make-rgba 0f0 1f0 0f0 1f0)))
    (scene:add-filled-rectangle list 100f0 80f0 120f0 60f0 red)
    (scene:add-line list 40f0 200.5f0 260f0 200.5f0 green)
    (scene:add-triangle list 400f0 100f0 500f0 100f0 400f0 200f0 red)
    (let ((frame (render-frame window renderer list))
          (background (color-octets *test-background*)))
      (list :background (frame-pixel frame 5 5)
            :rectangle-inside (frame-pixel frame 160 110)
            :rectangle-left-of (frame-pixel frame 96 110)
            :rectangle-right-of (frame-pixel frame 224 110)
            :rectangle-above (frame-pixel frame 160 76)
            :rectangle-below (frame-pixel frame 160 144)
            :triangle-inside (frame-pixel frame 420 120)
            :triangle-outside (frame-pixel frame 480 180)
            :line-ink (count-ink frame 150 194 151 208 background)
            :line-far-ink (count-ink frame 150 230 151 244 background)
            :frame-size (list (frame-width frame) (frame-height frame) (frame-scale frame))))))

(defun observe-text (window
                     renderer)
  "Render plain, accented and rotated text and count the pixels they cover.

WINDOW: the SDL window.
RENDERER: the renderer."
  (let ((list (scene:make-draw-list))
        (white (scene:make-rgba 1f0 1f0 1f0 1f0))
        (accented (coerce (list #\C #\a #\f (code-char 233) #\Space #\A (code-char 231) #\a (code-char 237))
                          'string)))
    (scene:add-text list "Hello" 60f0 270f0 24f0 white)
    (scene:add-text list accented 200f0 400f0 20f0 white)
    (scene:add-text list "Rotated" 700f0 300f0 20f0 white :angle (float (/ pi 2) 1f0))
    (scene:add-text list "Tiny" 900f0 500f0 6f0 white)
    (scene:add-text list "Huge" 900f0 640f0 120f0 white)
    (let ((frame (render-frame window renderer list))
          (background (color-octets *test-background*)))
      (list :plain-ink (count-ink frame 60 250 140 280 background)
            :plain-far-ink (count-ink frame 300 250 400 280 background)
            :accented-ink (count-ink frame 200 380 400 410 background)
            :rotated-ink (count-ink frame 680 200 710 305 background)
            :rotated-wrong-place-ink (count-ink frame 700 280 820 310 background)
            :tiny-ink (count-ink frame 900 490 930 505 background)
            :huge-ink (count-ink frame 900 540 1200 650 background)))))

(defun observe-interface-text (window
                                renderer)
  "Render interface text in both interface fonts and report where its ink is, against the width
the layout measured for it.

WINDOW: the SDL window.
RENDERER: the renderer."
  (let* ((list (scene:make-draw-list))
         (white (scene:make-rgba 1f0 1f0 1f0 1f0))
         (text "Properties and layers")
         (size 13f0)
         (regular-width (ui:text-width-pixels text size :ui))
         (strong-width (ui:text-width-pixels text size :ui-strong)))
    (scene:add-text list text 100f0 100f0 size white :font :ui)
    (scene:add-text list text 100f0 160f0 size white :font :ui-strong)
    (let* ((frame (render-frame window renderer list))
           (background (color-octets *test-background*))
           (regular-end (+ 102 (ceiling regular-width)))
           (strong-end (+ 102 (ceiling strong-width))))
      (list :regular-width regular-width
            :strong-width strong-width
            :regular-ink (count-ink frame 98 85 regular-end 106 background)
            :regular-past-the-end-ink (count-ink frame (+ regular-end 2) 85 (+ regular-end 62) 106 background)
            :strong-ink (count-ink frame 98 145 strong-end 166 background)
            :strong-past-the-end-ink (count-ink frame (+ strong-end 2) 145 (+ strong-end 62) 166 background)
            :measurer-installed (not (eq ui:*text-measurer* (function ui:fixed-advance-width)))))))

(defun observe-clipboard ()
  "Copy text to the system clipboard and read it back, with an accented text, a text with a line
break, and an empty one.

The clipboard belongs to the video system, so this runs inside the window session. It is also
the user's clipboard: the text that was on it is put back afterwards."
  (let ((original (platform:sdl-clipboard-text))
        (accented (format nil "Planta T~crrea ~c~c~c 12,5"
                          (code-char 233)
                          (code-char 231)
                          (code-char 227)
                          (code-char 111)))
        (lines (format nil "first~%second")))
    (unwind-protect
         (list :accepted (platform:set-sdl-clipboard-text accented)
               :accented (platform:sdl-clipboard-text)
               :expected-accented accented
               :lines-accepted (platform:set-sdl-clipboard-text lines)
               :lines (platform:sdl-clipboard-text)
               :expected-lines lines
               :empty-accepted (platform:set-sdl-clipboard-text "")
               :empty (platform:sdl-clipboard-text))
      (platform:set-sdl-clipboard-text (or original "")))))

(defun observe-application-frame (window
                                  renderer)
  "Render a whole application frame (canvas and panels) for a generated drawing.

WINDOW: the SDL window.
RENDERER: the renderer."
  (let* ((drawing (rng:generate-drawing :seed 5 :entity-count 60))
         (application (app:make-app (base:default-config)
                                    :drawing drawing
                                    :width (float +test-window-width+ 1d0)
                                    :height (float +test-window-height+ 1d0)))
         (theme (app:app-theme application)))
    (app:app-handle-event application (platform::resize-event-for window))
    (app:operation-zoom-extents application)
    (app:app-render application)
    (let* ((frame (render-frame window
                                renderer
                                (app:app-draw-list application)
                                :background (scene:theme-canvas-background theme)))
           (canvas-background (color-octets (scene:theme-canvas-background theme)))
           (panel-background (color-octets (scene:theme-panel-background theme))))
      (list :canvas-ink (count-ink frame 300 100 900 600 canvas-background)
            :panel-ink (count-ink frame 0 0 200 700 panel-background)
            :canvas-area (* 600 500)
            :frame-size (list (frame-width frame) (frame-height frame))
            :screenshot-path (let ((path (merge-pathnames "borba-cad-renderer-test.png"
                                                          (uiop:temporary-directory))))
                               (platform::save-screenshot path (frame-width frame) (frame-height frame))
                               path)))))

(defun observe-canvas-capture (window
                              renderer)
  "Take a picture of the canvas of an application with a generated drawing, as the export of an image does.

WINDOW: the SDL window.
RENDERER: the renderer."
  (let* ((drawing (rng:generate-drawing :seed 5 :entity-count 60))
         (application (app:make-app (base:default-config)
                                    :drawing drawing
                                    :width (float +test-window-width+ 1d0)
                                    :height (float +test-window-height+ 1d0)))
         (theme (app:app-theme application))
         (background (color-octets (scene:theme-canvas-background theme))))
    (app:app-handle-event application (platform::resize-event-for window))
    (app:operation-zoom-extents application)
    (multiple-value-bind (pixels width height) (platform::capture-canvas application window renderer)
      (multiple-value-bind (logical-width) (sdl2:get-window-size window)
        (let* ((scale (/ (float (platform::drawable-size window) 1d0) logical-width))
               (canvas (ui:layout-canvas (app:app-layout application)))
               (ink 0)
               (corner (and pixels (list (aref pixels 0) (aref pixels 1) (aref pixels 2)))))
          (when pixels
            (dotimes (index (* width height))
              (unless (same-color-p (list (aref pixels (* 3 index))
                                          (aref pixels (+ 1 (* 3 index)))
                                          (aref pixels (+ 2 (* 3 index))))
                                    background)
                (incf ink))))
          (list :width width
                :height height
                :expected-width (round (* scale (ui:rect-width canvas)))
                :expected-height (round (* scale (ui:rect-height canvas)))
                :window-width (round (* scale +test-window-width+))
                :octets (and pixels (length pixels))
                :ink ink
                :corner corner
                :background background
                :jpeg-bytes (and pixels
                                 (length (borba-cad/image:encode-jpeg width height pixels)))))))))

(defun picture-of-colors (width
                          height
                          colors)
  "Return a raster of WIDTH by HEIGHT whose quadrants are the four colors of COLORS: top left, top right, bottom left,
bottom right, each (red green blue).

WIDTH: width in pixels.
HEIGHT: height in pixels.
COLORS: the four colors."
  (let* ((raster (borba-cad/image:make-raster width height))
         (pixels (borba-cad/image:raster-pixels raster)))
    (dotimes (row height)
      (dotimes (column width)
        (let ((color (nth (+ (if (< column (floor width 2)) 0 1) (if (< row (floor height 2)) 0 2)) colors))
              (at (* 4 (+ column (* row width)))))
          (setf (aref pixels at) (first color)
                (aref pixels (+ at 1)) (second color)
                (aref pixels (+ at 2)) (third color)
                (aref pixels (+ at 3)) 255))))
    raster))

(defun observe-pictures (window
                         renderer)
  "Draw pictures through the renderer: one upright, one turned a quarter, one half opaque, one drawn again from the
texture the renderer kept, and one too wide for a texture.

WINDOW: the SDL window.
RENDERER: the renderer."
  (let* ((colors '((255 0 0) (0 255 0) (0 0 255) (255 255 0)))
         (picture (picture-of-colors 2 2 colors))
         (list (scene:make-draw-list)))
    (scene:add-picture list 9001 picture '((100f0 . 300f0) (300f0 . 300f0) (300f0 . 100f0) (100f0 . 100f0)) 1f0)
    (scene:add-picture list 9002 picture '((600f0 . 300f0) (600f0 . 100f0) (800f0 . 100f0) (800f0 . 300f0)) 1f0)
    (scene:add-picture list 9003 picture '((900f0 . 300f0) (1100f0 . 300f0) (1100f0 . 100f0) (900f0 . 100f0)) 0.5f0)
    (let ((first (render-frame window renderer list))
          (again (scene:make-draw-list)))
      (scene:add-picture again 9001 nil '((100f0 . 300f0) (300f0 . 300f0) (300f0 . 100f0) (100f0 . 100f0)) 1f0)
      (let* ((kept (render-frame window renderer again))
             (wide (borba-cad/image:make-raster 40000 2))
             (wide-list (scene:make-draw-list)))
        (let ((pixels (borba-cad/image:raster-pixels wide)))
          (dotimes (index (length pixels))
            (setf (aref pixels index) (if (= 3 (mod index 4)) 255 200))))
        (scene:add-picture wide-list 9004 wide '((100f0 . 600f0) (700f0 . 600f0) (700f0 . 500f0) (100f0 . 500f0)) 1f0)
        (let ((reduced (render-frame window renderer wide-list)))
          (list :upright-top-left (frame-pixel first 150 150)
                :upright-top-right (frame-pixel first 250 150)
                :upright-bottom-left (frame-pixel first 150 250)
                :upright-bottom-right (frame-pixel first 250 250)
                :outside (frame-pixel first 90 200)
                :turned-top-left (frame-pixel first 650 150)
                :turned-top-right (frame-pixel first 750 150)
                :turned-bottom-left (frame-pixel first 650 250)
                :turned-bottom-right (frame-pixel first 750 250)
                :half-opaque (frame-pixel first 950 150)
                :kept-top-left (frame-pixel kept 150 150)
                :kept-bottom-right (frame-pixel kept 250 250)
                :wide-inside (frame-pixel reduced 400 550)
                :wide-outside (frame-pixel reduced 400 450)
                :background (color-octets *test-background*)))))))

(defun observe-underlay-frame (window
                               renderer)
  "Render a whole application frame of an empty drawing with a picture under it, as the program draws one that is
opened, and count what the picture covers of the canvas.

WINDOW: the SDL window.
RENDERER: the renderer."
  (let* ((application (app:make-app (base:default-config)
                                    :width (float +test-window-width+ 1d0)
                                    :height (float +test-window-height+ 1d0)))
         (theme (app:app-theme application))
         (path (namestring (asdf:system-relative-pathname "borba-cad" "tests/fixtures/images/png-rgb8.png"))))
    (app:app-handle-event application (platform::resize-event-for window))
    (let ((added (app:add-underlay-from-app application path)))
      (app:app-render application)
      (let* ((frame (render-frame window
                                  renderer
                                  (app:app-draw-list application)
                                  :background (scene:theme-canvas-background theme)))
             (canvas-background (color-octets (scene:theme-canvas-background theme)))
             (underlay (first (dom:drawing-underlays (app:app-drawing application))))
             (viewport (app:app-viewport application)))
        (multiple-value-bind (left top)
            (view:world->screen viewport
                                (dom:underlay-x underlay)
                                (+ (dom:underlay-y underlay) (dom:underlay-height underlay)))
          (multiple-value-bind (right bottom)
              (view:world->screen viewport
                                  (+ (dom:underlay-x underlay) (dom:underlay-width underlay))
                                  (dom:underlay-y underlay))
            (list :added added
                  :picture-runs (length (scene:draw-list-picture-runs (app:app-draw-list application)))
                  :inside-ink (count-ink frame
                                         (ceiling (+ left 4))
                                         (ceiling (+ top 4))
                                         (floor (- right 4))
                                         (floor (- bottom 4))
                                         canvas-background)
                  :inside-area (* (- (floor (- right 4)) (ceiling (+ left 4)))
                                  (- (floor (- bottom 4)) (ceiling (+ top 4))))
                  :outside-ink (count-ink frame
                                          (floor (- left 50))
                                          (ceiling top)
                                          (floor (- left 10))
                                          (floor bottom)
                                          canvas-background)
                  :outside-area (* 40 (- (floor bottom) (ceiling top))))))))))

(defun collect-observations ()
  "Open one window and take every observation of the renderer tests in it."
  (platform::call-with-gl-window
   (base:default-config)
   +test-window-width+
   +test-window-height+
   "Borba CAD renderer tests"
   (lambda (window renderer phases)
     (list :startup phases
           :shapes (observe-shapes window renderer)
           :text (observe-text window renderer)
           :interface-text (observe-interface-text window renderer)
           :clipboard (observe-clipboard)
           :application (observe-application-frame window renderer)
           :capture (observe-canvas-capture window renderer)
           :pictures (observe-pictures window renderer)
           :underlay-frame (observe-underlay-frame window renderer)))))

(defun observation (&rest path)
  "Return a value from the shared observations, taking them on first use.

PATH: keywords leading to the value, for example :shapes :background."
  (unless *observations*
    (setf *observations* (collect-observations)))
  (let ((value *observations*))
    (dolist (key path value)
      (setf value (getf value key)))))

(test flat-shapes-cover-exactly-their-area
  (when (display-or-skip)
    (let ((red '(255 0 0)))
      (is (same-color-p (observation :shapes :background) (color-octets *test-background*)))
      (is (same-color-p (observation :shapes :rectangle-inside) red))
      (is (same-color-p (observation :shapes :rectangle-left-of) (color-octets *test-background*)))
      (is (same-color-p (observation :shapes :rectangle-right-of) (color-octets *test-background*)))
      (is (same-color-p (observation :shapes :rectangle-above) (color-octets *test-background*)))
      (is (same-color-p (observation :shapes :rectangle-below) (color-octets *test-background*)))
      (is (same-color-p (observation :shapes :triangle-inside) red))
      (is (same-color-p (observation :shapes :triangle-outside) (color-octets *test-background*))))))

(test a-line-is-drawn-where-it-was-asked-and-nowhere-else
  (when (display-or-skip)
    (is (>= (observation :shapes :line-ink) 1))
    (is (= 0 (observation :shapes :line-far-ink)))))

(test text-leaves-ink-in-its-box-and-nowhere-else
  (when (display-or-skip)
    (is (> (observation :text :plain-ink) 40))
    (is (= 0 (observation :text :plain-far-ink)))))

(test accented-characters-have-glyphs
  (when (display-or-skip)
    (is (> (observation :text :accented-ink) 60))))

(test rotated-text-follows-its-angle
  (when (display-or-skip)
    (is (> (observation :text :rotated-ink) 30) "no ink along the vertical baseline")
    (is (< (observation :text :rotated-wrong-place-ink) (observation :text :rotated-ink))
        "rotated text must not be laid out horizontally")))

(test interface-text-ends-where-the-layout-says-it-does
  (when (display-or-skip)
    (is (observation :interface-text :measurer-installed) "the window installs the real measurer")
    (is (> (observation :interface-text :regular-ink) 100))
    (is (> (observation :interface-text :strong-ink) 100))
    (is (= 0 (observation :interface-text :regular-past-the-end-ink)) "no ink past the measured width")
    (is (= 0 (observation :interface-text :strong-past-the-end-ink)))
    (is (> (observation :interface-text :strong-width) (observation :interface-text :regular-width))
        "the strong face is wider")))

(test the-system-clipboard-round-trips-text-including-accents-and-lines-and-reads-empty-as-nothing
  (when (display-or-skip)
    (is (observation :clipboard :accepted))
    (is (string= (observation :clipboard :expected-accented) (observation :clipboard :accented)))
    (is (string= (observation :clipboard :expected-lines) (observation :clipboard :lines)))
    (is (null (observation :clipboard :empty)) "an empty clipboard is no text")))

(test the-measurer-is-removed-when-the-window-closes
  (when (display-or-skip)
    (observation :interface-text :regular-width)
    (is (eq (function ui:fixed-advance-width) ui:*text-measurer*))))

(test text-is-drawn-at-both-extreme-sizes
  (when (display-or-skip)
    (is (> (observation :text :tiny-ink) 0))
    (is (> (observation :text :huge-ink) 1000))))

(test a-whole-application-frame-draws-the-canvas-and-the-panels
  (when (display-or-skip)
    (is (> (observation :application :canvas-ink) 200) "the drawing leaves no ink on the canvas")
    (is (< (observation :application :canvas-ink) (observation :application :canvas-area))
        "the canvas background must show between the entities")
    (is (> (observation :application :panel-ink) 200) "the panels leave no ink")))

(test the-screenshot-is-a-valid-png-of-the-framebuffer-size
  (when (display-or-skip)
    (let* ((path (observation :application :screenshot-path))
           (size (observation :application :frame-size)))
      (unwind-protect
           (let* ((chunks (png-chunks (read-octets path)))
                  (header (second (first chunks))))
             (is (every #'third chunks))
             (is (= (first size) (big-endian-at header 0)))
             (is (= (second size) (big-endian-at header 4))))
        (uiop:delete-file-if-exists path)))))

(test the-frame-benchmark-measures-every-frame
  (when (display-or-skip)
    (let ((result (platform:run-frame-benchmark (rng:generate-drawing :seed 6 :entity-count 100)
                                                :frames 12
                                                :warmup 2)))
      (is (= 12 (platform:frame-benchmark-result-frames result)))
      (dolist (summary (list (platform:frame-benchmark-result-cpu-ms result)
                             (platform:frame-benchmark-result-gpu-wait-ms result)
                             (platform:frame-benchmark-result-swap-ms result)))
        (is (= 12 (obs:sample-summary-count summary)))
        (is (plusp (obs:sample-summary-max summary))))
      (is (plusp (length (platform:frame-benchmark-result-gl-renderer result)))))))

(test the-picture-of-the-canvas-is-the-canvas-and-nothing-of-the-panels
  (when (display-or-skip)
    (is (= (observation :capture :expected-width) (observation :capture :width)))
    (is (= (observation :capture :expected-height) (observation :capture :height)))
    (is (< (observation :capture :width) (observation :capture :window-width)) "the panels are left out")
    (is (= (* 3 (observation :capture :width) (observation :capture :height)) (observation :capture :octets)))))

(test the-picture-of-the-canvas-has-its-background-and-its-drawing
  (when (display-or-skip)
    (is (same-color-p (observation :capture :corner) (observation :capture :background))
        "the top left corner is the background of the canvas, not a panel")
    (is (> (observation :capture :ink) 200) "the drawing left ink")
    (is (< (observation :capture :ink)
           (* (observation :capture :width) (observation :capture :height)))
        "and the background shows between the entities")))

(test the-picture-of-the-canvas-encodes-as-a-jpeg
  (when (display-or-skip)
    (is (> (observation :capture :jpeg-bytes) 500))))

(test a-picture-is-drawn-upright-with-its-corners-where-they-were-given
  (when (display-or-skip)
    (is (same-color-p (observation :pictures :upright-top-left) '(255 0 0)))
    (is (same-color-p (observation :pictures :upright-top-right) '(0 255 0)))
    (is (same-color-p (observation :pictures :upright-bottom-left) '(0 0 255)))
    (is (same-color-p (observation :pictures :upright-bottom-right) '(255 255 0)))
    (is (same-color-p (observation :pictures :outside) (observation :pictures :background)))))

(test a-picture-turns-with-its-corners
  (when (display-or-skip)
    (is (same-color-p (observation :pictures :turned-top-left) '(255 255 0)))
    (is (same-color-p (observation :pictures :turned-top-right) '(0 255 0)))
    (is (same-color-p (observation :pictures :turned-bottom-left) '(0 0 255)))
    (is (same-color-p (observation :pictures :turned-bottom-right) '(255 0 0)))))

(test a-picture-with-an-opacity-lets-what-is-behind-it-through
  (when (display-or-skip)
    (let ((background (observation :pictures :background))
          (shown (observation :pictures :half-opaque)))
      (is (same-color-p shown (mapcar (lambda (paint behind) (round (+ (* 0.5 paint) (* 0.5 behind))))
                                      '(255 0 0)
                                      background))))))

(test a-picture-is-drawn-again-from-the-texture-the-renderer-kept
  (when (display-or-skip)
    (is (same-color-p (observation :pictures :kept-top-left) '(255 0 0)))
    (is (same-color-p (observation :pictures :kept-bottom-right) '(255 255 0)))))

(test a-picture-wider-than-a-texture-may-be-is-reduced-and-still-drawn
  (when (display-or-skip)
    (is (same-color-p (observation :pictures :wide-inside) '(200 200 200)))
    (is (same-color-p (observation :pictures :wide-outside) (observation :pictures :background)))))

(test a-picture-under-the-drawing-shows-on-the-canvas-of-the-application
  (when (display-or-skip)
    (is (observation :underlay-frame :added))
    (is (= 1 (observation :underlay-frame :picture-runs)))
    (is (> (observation :underlay-frame :inside-ink) (* 0.9 (observation :underlay-frame :inside-area)))
        "the picture covers the rectangle it stands on")
    (is (< (observation :underlay-frame :outside-ink) (* 0.1 (observation :underlay-frame :outside-area)))
        "and nothing beside it")))
