(in-package #:borba-cad/tests)

(def-suite theme
  :description "The light and the dark theme, the switch between them, and the buttons that zoom."
  :in all)

(in-suite theme)

;;; Helpers

(defun theme-app (directory
                  &key option)
  "Return an application whose state is kept in DIRECTORY and whose configuration asks for a theme, when OPTION is
given.

DIRECTORY: the folder where what is remembered between runs is kept.
OPTION: :dark or :light, as the option --theme would say, or NIL."
  (let ((config (quiet-config)))
    (setf (base:config-state-directory config) directory
          (base:config-theme config) option)
    (app:make-app config)))

(defun lightness-of (color)
  "Return the lightness of an RGBA, from 0 to 1.

COLOR: an RGBA."
  (nth-value 2 (scene::rgb->hsl (scene:rgba-r color)
                                (scene:rgba-g color)
                                (scene:rgba-b color))))

(defun canvas-center-world (application)
  "Return the point of the drawing at the center of the canvas, as a list of x and y.

APPLICATION: the application."
  (let ((canvas (canvas-of application)))
    (multiple-value-list
     (view:screen->world (app:app-viewport application)
                         (float (+ (ui:rect-x canvas) (/ (ui:rect-width canvas) 2)) 1d0)
                         (float (+ (ui:rect-y canvas) (/ (ui:rect-height canvas) 2)) 1d0)))))

;;; The themes

(test a-theme-is-found-by-its-name
  (is (eq :dark (scene:theme-kind (scene:theme-named :dark))))
  (is (eq :light (scene:theme-kind (scene:theme-named "light"))))
  (is (eq :light (scene:theme-kind (scene:theme-named "LIGHT"))))
  (is (null (scene:theme-named "solarized")))
  (is (null (scene:theme-named :nothing)))
  (is (string= "dark" (scene:theme-name (scene:default-theme))))
  (is (string= "light" (scene:theme-name (scene:light-theme)))))

(test the-light-theme-has-a-white-canvas-and-dark-text
  (let ((light (scene:light-theme))
        (dark (scene:default-theme)))
    (is (> (lightness-of (scene:theme-canvas-background light)) 0.9))
    (is (< (lightness-of (scene:theme-canvas-background dark)) 0.1))
    (is (< (lightness-of (scene:theme-text light)) 0.2))
    (is (> (lightness-of (scene:theme-text dark)) 0.8))
    (is (> (- (lightness-of (scene:theme-canvas-background light))
              (lightness-of (scene:theme-text light)))
           0.7)
        "text on the canvas can be read")))

(test the-dark-canvas-draws-the-colors-of-the-layers-as-they-are
  (let ((color (scene:make-rgba 0.92 0.92 0.92 1f0)))
    (is (eq color (scene:theme-ink (scene:default-theme) color)))))

(test the-light-canvas-darkens-what-is-too-pale-to-see
  (let* ((theme (scene:light-theme))
         (white (scene:theme-ink theme (scene:make-rgba 0.92 0.92 0.92 1f0)))
         (yellow (scene:theme-ink theme (scene:make-rgba 1.0 0.86 0.39 0.5)))
         (blue-pale (scene:theme-ink theme (scene:make-rgba 0.47 0.78 1.0 1f0))))
    (is (< (lightness-of white) 0.13) "a near-white layer is near-black on paper")
    (is (< (lightness-of yellow) 0.41))
    (is (= 0.5 (scene:rgba-a yellow)) "the opacity is kept")
    (is (> (scene:rgba-r yellow) (scene:rgba-b yellow)) "the hue is kept: yellow stays warm")
    (is (< (lightness-of blue-pale) 0.41))
    (is (> (scene:rgba-b blue-pale) (scene:rgba-r blue-pale)) "blue stays blue")))

(test the-light-canvas-leaves-a-dark-color-alone
  (let ((theme (scene:light-theme))
        (color (scene:make-rgba 0.1 0.2 0.6 1f0)))
    (is (eq color (scene:theme-ink theme color)))))

(test a-color-goes-through-hue-saturation-and-lightness-and-back
  (dolist (channels '((0.2 0.4 0.8) (1.0 0.5 0.0) (0.3 0.3 0.3) (0.9 0.1 0.4) (0.0 0.7 0.5)))
    (multiple-value-bind (hue saturation lightness) (apply #'scene::rgb->hsl channels)
      (multiple-value-bind (red green blue) (scene::hsl->rgb hue saturation lightness)
        (is (every (lambda (expected actual) (< (abs (- expected actual)) 1f-5))
                   channels
                   (list red green blue))
            "~a" channels)))))

(test layers-are-drawn-in-the-ink-of-the-theme
  (let ((drawing (dom:make-drawing)))
    (let ((dark (gethash dom:+default-layer-id+ (scene::layer-styles drawing (scene:default-theme))))
          (light (gethash dom:+default-layer-id+ (scene::layer-styles drawing (scene:light-theme)))))
      (is (> (lightness-of (cdr dark)) 0.8))
      (is (< (lightness-of (cdr light)) 0.2)))))

;;; Choosing a theme

(test the-theme-is-an-option-and-a-setting-that-is-remembered
  (let ((base (base:default-config :project-root #p"/proj/")))
    (is (eq :light (base:config-theme (base:config-with-arguments base '("--theme" "light")))))
    (is (eq :dark (base:config-theme (base:config-with-arguments base '("--theme" "DARK")))))
    (is (null (base:config-theme base)))
    (signals base:configuration-error (base:config-with-arguments base '("--theme" "sepia")))
    (signals base:configuration-error (base:config-with-arguments base '("--theme")))))

(test the-switch-changes-the-theme-and-it-is-remembered-for-the-next-run
  (with-temporary-directory (directory)
    (let ((application (theme-app directory)))
      (is (eq :dark (scene:theme-kind (app:app-theme application))) "nothing saved means dark")
      (app:operation-toggle-theme application)
      (is (eq :light (scene:theme-kind (app:app-theme application))))
      (is (string= "Light theme." (message-text application)))
      (is (equal "light" (persist:load-setting directory "theme")))
      (is (eq :light (scene:theme-kind (app:app-theme (theme-app directory)))) "the next run starts light")
      (app:operation-toggle-theme application)
      (is (eq :dark (scene:theme-kind (app:app-theme application))))
      (is (equal "dark" (persist:load-setting directory "theme"))))))

(test an-option-beats-the-saved-theme
  (with-temporary-directory (directory)
    (persist:save-setting directory "theme" "light")
    (is (eq :dark (scene:theme-kind (app:app-theme (theme-app directory :option :dark)))))
    (is (eq :light (scene:theme-kind (app:app-theme (theme-app directory)))))
    (persist:save-setting directory "theme" "sepia")
    (is (eq :dark (scene:theme-kind (app:app-theme (theme-app directory)))) "a theme that is not known is ignored")))

(test the-title-bar-has-a-button-that-switches-the-theme
  (let ((application (make-test-app)))
    (is (find-widget application :theme))
    (is (eq :sun (ui:widget-icon (find-widget application :theme))) "the dark theme offers the light one")
    (is (string= "Switch to the light theme" (ui:widget-hint (find-widget application :theme))))
    (click-widget application :theme)
    (is (eq :light (scene:theme-kind (app:app-theme application))))
    (is (eq :moon (ui:widget-icon (find-widget application :theme))))
    (is (string= "Switch to the dark theme" (ui:widget-hint (find-widget application :theme))))
    (click-widget application :theme)
    (is (eq :dark (scene:theme-kind (app:app-theme application))))))

(test the-whole-window-can-be-drawn-in-the-light-theme
  (let ((application (make-test-app)))
    (app:operation-toggle-theme application)
    (is (not (null (app:app-render application))))
    (is (> (lightness-of (scene:theme-canvas-background (app:app-theme application))) 0.9))))

;;; Zoom

(test the-buttons-that-zoom-are-in-the-top-right-corner-of-the-canvas
  (let* ((application (make-test-app))
         (canvas (canvas-of application))
         (zoom-in (find-widget application :zoom-in))
         (zoom-out (find-widget application :zoom-out)))
    (is (not (null zoom-in)))
    (is (not (null zoom-out)))
    (is (<= (+ (ui:widget-x zoom-in) (ui:widget-width zoom-in))
            (+ (ui:rect-x canvas) (ui:rect-width canvas))))
    (is (< (- (+ (ui:rect-x canvas) (ui:rect-width canvas)) (ui:widget-x zoom-in)) 60))
    (is (< (- (ui:widget-y zoom-in) (ui:rect-y canvas)) 30))
    (is (> (ui:widget-y zoom-out) (ui:widget-y zoom-in)) "out is under in")
    (is (= (ui:widget-x zoom-in) (ui:widget-x zoom-out)))
    (is (string= "Zoom in" (ui:widget-hint zoom-in)))
    (is (string= "Zoom out" (ui:widget-hint zoom-out)))))

(test a-button-zooms-by-one-notch-of-the-wheel-about-the-center-of-the-canvas
  (let* ((application (make-test-app))
         (scale (view:viewport-scale (app:app-viewport application)))
         (center (canvas-center-world application)))
    (click-widget application :zoom-in)
    (is (approx= (* scale app:+zoom-step-per-notch+) (view:viewport-scale (app:app-viewport application))))
    (is (every #'approx= center (canvas-center-world application)) "the center of the canvas stays where it was")
    (click-widget application :zoom-out)
    (is (approx= scale (view:viewport-scale (app:app-viewport application))))
    (click-widget application :zoom-out)
    (is (approx= (/ scale app:+zoom-step-per-notch+) (view:viewport-scale (app:app-viewport application))))))

(test the-buttons-that-zoom-do-not-draw-or-select-under-them
  (let ((application (make-test-app)))
    (click-widget application :zoom-in)
    (is (eq :select (app:tool-kind (app:app-tool application))))
    (is (zerop (length (entities-of application))))
    (is (null (app:app-window application)))))

;;; The zoom of the interface

(test the-interface-zoom-steps-through-the-zooms-and-stops-at-the-ends
  (let ((application (make-test-app)))
    (is (= 1d0 (app:app-interface-zoom application)))
    (app:operation-interface-zoom-in application)
    (is (= 1.1d0 (app:app-interface-zoom application)))
    (app:operation-interface-zoom-in application)
    (is (= 1.25d0 (app:app-interface-zoom application)))
    (is (search "125%" (message-text application)))
    (dotimes (n 10)
      (app:operation-interface-zoom-in application))
    (is (= 2d0 (app:app-interface-zoom application)) "it stops at the largest")
    (app:operation-interface-zoom-reset application)
    (is (= 1d0 (app:app-interface-zoom application)))
    (dotimes (n 10)
      (app:operation-interface-zoom-out application))
    (is (= 0.75d0 (app:app-interface-zoom application)) "and at the smallest")))

(test the-zoom-of-the-interface-is-remembered-for-the-next-run
  (with-temporary-directory (directory)
    (let ((application (theme-app directory)))
      (app:operation-interface-zoom-in application)
      (is (equal "110" (persist:load-setting directory "interface-zoom")))
      (is (= 1.1d0 (app:app-interface-zoom (theme-app directory)))))
    (persist:save-setting directory "interface-zoom" "137")
    (is (= 1d0 (app:app-interface-zoom (theme-app directory))) "a zoom the keys do not give is ignored")
    (persist:save-setting directory "interface-zoom" "big")
    (is (= 1d0 (app:app-interface-zoom (theme-app directory))))))

(test control-and-plus-minus-or-zero-zoom-the-interface
  (let ((application (make-test-app)))
    (press application :plus :control)
    (is (= 1.1d0 (app:app-interface-zoom application)))
    (press application :minus :control)
    (is (= 1d0 (app:app-interface-zoom application)))
    (press application :plus :control)
    (press application :zero :control)
    (is (= 1d0 (app:app-interface-zoom application)))
    (press application :plus)
    (is (= 1d0 (app:app-interface-zoom application)) "without Control the key is not a shortcut")))
