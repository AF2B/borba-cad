(in-package #:borba-cad/platform-tests)

(def-suite fonts
  :description "Font selection and glyph sizes."
  :in all)

(in-suite fonts)

(test small-text-is-rasterized-at-its-exact-size
  (is (= 4 (platform:glyph-raster-size 1)))
  (is (= 4 (platform:glyph-raster-size 4)))
  (is (= 12 (platform:glyph-raster-size 12)))
  (is (= 12 (platform:glyph-raster-size 11.6)))
  (is (= 32 (platform:glyph-raster-size 32))))

(test large-text-uses-a-ladder-of-sizes-and-is-capped
  (let ((sizes (loop for physical from 33 to 400 collect (platform:glyph-raster-size physical))))
    (is (every #'<= sizes (rest sizes)) "the raster size never decreases with the text size")
    (is (every (lambda (size physical) (or (>= size physical) (= size 256)))
               sizes
               (loop for physical from 33 to 400 collect physical))
        "a glyph is never rasterized smaller than needed, up to the cap")
    (is (<= (length (remove-duplicates sizes)) 12) "the ladder keeps the number of sizes small")
    (is (= 256 (platform:glyph-raster-size 1000)))))

(test an-explicit-font-path-must-exist
  (let ((config (base:default-config)))
    (setf (base:config-font-path config) "/nonexistent/font.ttf")
    (signals base:configuration-error (platform:find-font-path config))))

(test the-default-search-finds-a-readable-font-or-says-what-to-install
  (let ((config (base:default-config)))
    (handler-case
        (let ((path (platform:find-font-path config)))
          (is (probe-file path))
          (is (member (pathname-type path) '("ttf" "otf") :test #'string-equal)))
      (base:configuration-error (condition)
        (is (search "no monospace font" (princ-to-string condition)))))))

(defun usable-font-paths ()
  "Return the font files of every role, or NIL when the machine has no usable monospace font."
  (handler-case (platform:find-font-paths (base:default-config))
    (base:configuration-error () nil)))

(test every-font-role-gets-a-readable-file
  (let ((paths (usable-font-paths)))
    (when paths
      (dolist (role '(:mono :ui :ui-strong))
        (is (probe-file (getf paths role)) "~s has a font file" role)))))

(test the-interface-fonts-fall-back-to-the-monospace-font-when-none-is-installed
  (let ((paths (usable-font-paths)))
    (when paths
      (is (stringp (getf paths :ui)))
      (is (stringp (getf paths :ui-strong))))))

(defmacro with-test-fonts ((fonts) &body body)
  "Run BODY with FONTS bound to the opened font set, or skip it when no font is installed."
  `(let ((paths (usable-font-paths)))
     (when paths
       (let ((,fonts (platform::open-font-set paths)))
         ,@body))))

(test interface-text-is-measured-from-the-advances-of-the-glyphs
  (with-test-fonts (fonts)
    (flet ((width (role string) (platform::font-set-text-width fonts role string 100f0)))
      (is (< (width :ui "iiii") (width :ui "mmmm")) "a narrow letter is narrower than a wide one")
      (is (= 0f0 (width :ui "")))
      (is (> (width :ui "abc") 0f0))
      (is (< (abs (- (width :ui "abcabc") (* 2 (width :ui "abc")))) 1f-3) "widths add up")
      (is (>= (width :ui-strong "Layers") (width :ui "Layers")) "the strong face is not narrower")
      (is (> (width :ui (coerce (list (code-char 233) (code-char 231)) 'string)) 0f0)
          "accented letters have advances"))))

(test the-width-of-text-scales-with-the-size
  (with-test-fonts (fonts)
    (let ((small (platform::font-set-text-width fonts :ui "Properties" 10f0))
          (large (platform::font-set-text-width fonts :ui "Properties" 40f0)))
      (is (< (abs (- (* 4 small) large)) 1f-2)))))

(test drawing-text-keeps-the-fixed-grid-of-the-geometry-module
  (with-test-fonts (fonts)
    (is (= (ui:fixed-advance-width :mono "0123456789" 20f0)
           (platform::font-set-text-width fonts :mono "0123456789" 20f0)))
    (is (= (* 3 geo:+text-advance-ratio+ 10) (platform::font-set-text-width fonts :mono "iii" 10)))))

(test the-measurer-of-a-font-set-answers-for-every-role
  (with-test-fonts (fonts)
    (let ((measurer (platform::font-set-measurer fonts)))
      (dolist (role '(:mono :ui :ui-strong))
        (is (> (funcall measurer role "Borba CAD" 13f0) 0f0))))))
