(defpackage #:borba-cad/platform
  (:use #:cl)
  (:local-nicknames (#:base #:borba-cad/base)
                    (#:sbcl #:borba-cad/sbcl-support)
                    (#:geo #:borba-cad/geometry)
                    (#:dom #:borba-cad/domain)
                    (#:view #:borba-cad/viewport)
                    (#:obs #:borba-cad/observability)
                    (#:scene #:borba-cad/scene)
                    (#:ui #:borba-cad/ui)
                    (#:app #:borba-cad/app)
                    (#:persist #:borba-cad/persistence))
  (:documentation "The only package that touches SDL, OpenGL and FreeType: window and event loop,
glyph atlas, the OpenGL renderer of the draw list, event translation and the entry point.")
  (:export
   #:main
   #:executable-main
   #:run-application
   #:parse-platform-options
   #:find-font-path
   #:translate-sdl-event
   #:keycode->key
   #:write-rgb-png
   #:run-frame-benchmark
   #:frame-benchmark-result
   #:frame-benchmark-result-frames
   #:frame-benchmark-result-cpu-ms
   #:frame-benchmark-result-gpu-wait-ms
   #:frame-benchmark-result-swap-ms
   #:frame-benchmark-result-interval-ms
   #:frame-benchmark-result-gl-renderer
   #:frame-benchmark-result-swap-interval
   #:glyph-raster-size
   #:display-available-p))
