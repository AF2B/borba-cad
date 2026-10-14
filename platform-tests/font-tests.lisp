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
