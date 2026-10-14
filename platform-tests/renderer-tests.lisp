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
           :application (observe-application-frame window renderer)))))

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
