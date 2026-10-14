(in-package #:borba-cad/platform)

;;;; The window, the event loop and the program's entry point.

(defconstant +gl-major-version+ 3
  "OpenGL context version requested: 3.3 core, available on every desktop GPU of the last decade.")

(defconstant +gl-minor-version+ 3
  "Minor part of the requested OpenGL version.")

(defconstant +minimum-window-width+ 1200
  "Smallest window width, so the toolbar always fits.")

(defconstant +minimum-window-height+ 700
  "Smallest window height.")

(defconstant +idle-wait-milliseconds+ 100
  "How long the loop sleeps waiting for an event when nothing needs redrawing.")

(defconstant +overlay-idle-wait-milliseconds+ 250
  "Idle wait while the performance overlay is visible, so its figures keep refreshing.")

(defconstant +demo-frames-per-step+ 3
  "Frames between two steps of the scripted demonstration.")

(defconstant +screenshot-settle-frames+ 4
  "Frames rendered after the last demonstration step before the screenshot is taken.")

(defconstant +no-vsync-sleep-seconds+ 0.004
  "Pause per frame when vertical sync is unavailable, to avoid spinning the CPU.")

(defparameter *usage*
  "Usage: borba-cad [options]
  --file PATH            open a drawing at startup
  --log-dir DIR          directory of the rotating log (default: var/log in the project)
  --state-dir DIR        directory of what is remembered between runs (default: var/state)
  --drawings-dir DIR     folder where the file dialogs start (default: your documents folder)
  --log-level LEVEL      error, warn, info, debug or trace (default: info)
  --log-stderr           also print log lines on standard error
  --log-sample EVENT=N   write only one in N entries of an event
  --font FILE            monospace TrueType font (default: Noto Sans Mono, DejaVu Sans Mono, ...)
  --width N, --height N  initial window size in logical pixels
  --demo                 run the scripted demonstration drawing
  --exit-after-demo      close the window when the demonstration ends
  --screenshot FILE      save the last frame of the demonstration as a PNG, then exit
  --help                 show this help
Environment: BORBA_CAD_ROOT, BORBA_CAD_LOG_DIR, BORBA_CAD_STATE_DIR,
BORBA_CAD_DRAWINGS_DIR, BORBA_CAD_LOG_LEVEL, BORBA_CAD_LOG_MAX_BYTES,
BORBA_CAD_LOG_MAX_FILES, BORBA_CAD_LOG_STDERR, BORBA_CAD_LOG_SAMPLING, BORBA_CAD_FONT,
BORBA_CAD_DEBUG (1: do not catch unexpected errors, for use with a debugger)."
  "Command-line help text.")

(defstruct (platform-options (:constructor make-platform-options (&key demo
                                                                       exit-after-demo
                                                                       screenshot
                                                                       help)))
  "Options that only the window program understands.

DEMO: run the scripted demonstration.
EXIT-AFTER-DEMO: close the window when it ends.
SCREENSHOT: file to save the last frame to, or NIL.
HELP: print the usage text and exit."
  demo exit-after-demo screenshot help)

(defun parse-platform-options (arguments)
  "Interpret the arguments left over by the core option parser. Signals CONFIGURATION-ERROR for
anything unknown.

ARGUMENTS: list of argument strings."
  (let ((options (make-platform-options))
        (queue arguments))
    (loop while queue
          do (let ((argument (pop queue)))
               (cond ((string= argument "--demo")
                      (setf (platform-options-demo options) t))
                     ((string= argument "--exit-after-demo")
                      (setf (platform-options-exit-after-demo options) t))
                     ((string= argument "--screenshot")
                      (unless queue
                        (error 'base:configuration-error :reason "--screenshot needs a file name"))
                      (setf (platform-options-screenshot options) (pop queue)
                            (platform-options-demo options) t
                            (platform-options-exit-after-demo options) t))
                     ((member argument '("--help" "-h") :test #'string=)
                      (setf (platform-options-help options) t))
                     (t (error 'base:configuration-error
                               :reason (format nil "unknown option ~s" argument))))))
    options))

(defun display-available-p ()
  "Return true when a graphical session is available (Wayland or X11)."
  (flet ((set-p (name)
           (let ((value (sbcl:environment-variable name)))
             (and value (plusp (length value))))))
    (or (set-p "WAYLAND_DISPLAY") (set-p "DISPLAY"))))

(defun phase-milliseconds (start
                           end)
  "Return the duration between two monotonic readings, in milliseconds.

START: reading at the beginning of the phase, in nanoseconds.
END: reading at its end, in nanoseconds."
  (obs:nanoseconds->milliseconds (- end start)))

(defun call-with-gl-window (config
                            width
                            height
                            title
                            function
                            &key (swap-interval 1))
  "Initialize SDL, create a window with an OpenGL 3.3 core context (4x multisampling when the
driver offers it), vertical sync and the renderer, call FUNCTION, and clean up.
FUNCTION receives the window, the renderer and a property list of startup phase durations.

CONFIG: the application configuration.
WIDTH: initial window width in logical pixels.
HEIGHT: initial window height in logical pixels.
TITLE: initial window title.
FUNCTION: function of the window, the renderer and the phase list.
SWAP-INTERVAL: 1 to wait for the display refresh at each buffer swap, 0 to swap at once."
  (let ((font-paths (find-font-paths config))
        (begin (sbcl:monotonic-nanoseconds))
        (failure nil)
        (result nil))
    ;; SDL2:WITH-INIT runs its body in a dedicated thread, where handlers established by the
    ;; caller do not apply. Errors are therefore caught there and re-signaled in the calling
    ;; thread, so the caller's handlers (logging, exit codes) see them. In debug mode they are
    ;; left alone so that a debugger can take them.
    (sdl2:with-init (:video)
      (handler-case
          (progn
            (sdl2:gl-set-attr :context-major-version +gl-major-version+)
            (sdl2:gl-set-attr :context-minor-version +gl-minor-version+)
            (sdl2:gl-set-attr :context-profile-mask sdl2-ffi:+sdl-gl-context-profile-core+)
            (sdl2:gl-set-attr :doublebuffer 1)
            (labels ((open-window (multisample)
                       (sdl2:gl-set-attr :multisamplebuffers (if multisample 1 0))
                       (sdl2:gl-set-attr :multisamplesamples (if multisample +multisample-count+ 0))
                       (sdl2:create-window :title title :w width :h height
                                           :flags '(:shown :opengl :resizable :allow-highdpi))))
              (let ((window (handler-case (open-window t)
                              (error () (open-window nil)))))
                (unwind-protect
                     (let* ((window-ready (sbcl:monotonic-nanoseconds))
                            (context (sdl2:gl-create-context window)))
                       (unwind-protect
                            (progn
                              (sdl2:gl-make-current window context)
                              (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl2:gl-get-proc-address)
                              (sdl2-ffi.functions:sdl-set-window-minimum-size window +minimum-window-width+
                                                                              +minimum-window-height+)
                              (let* ((vsync (handler-case (progn (sdl2:gl-set-swap-interval swap-interval) t)
                                              (error () nil)))
                                     (context-ready (sbcl:monotonic-nanoseconds))
                                     (renderer (make-renderer font-paths))
                                     (renderer-ready (sbcl:monotonic-nanoseconds)))
                                (unwind-protect
                                     (progn
                                       ;; Layout measures text with the glyph advances of the very
                                       ;; fonts that draw it.
                                       (setf ui:*text-measurer* (renderer-text-measurer renderer))
                                       (setf result
                                             (funcall function
                                                      window
                                                      renderer
                                                      (list :window-ms (phase-milliseconds begin window-ready)
                                                            :context-ms (phase-milliseconds window-ready
                                                                                            context-ready)
                                                            :renderer-ms (phase-milliseconds context-ready
                                                                                             renderer-ready)
                                                            :vsync vsync))))
                                  (setf ui:*text-measurer* #'ui:fixed-advance-width)
                                  (close-renderer renderer))))
                         (sdl2:gl-delete-context context)))
                  (sdl2:destroy-window window)))))
        (error (condition)
          (if (debug-mode-p)
              (error condition)
              (setf failure condition)))))
    (when failure
      (error failure))
    result))

(defun present-frame (window
                      renderer
                      list
                      background)
  "Render the draw list into the window's back buffer; returns the logical size and the display
scale as three values.

WINDOW: the SDL window.
RENDERER: the renderer.
LIST: the draw list.
BACKGROUND: an RGBA clear color."
  (multiple-value-bind (logical-width logical-height) (sdl2:get-window-size window)
    (multiple-value-bind (framebuffer-width framebuffer-height) (drawable-size window)
      (let ((scale (if (plusp logical-width) (/ (float framebuffer-width 1d0) logical-width) 1d0)))
        (render-draw-list renderer
                          list
                          logical-width
                          logical-height
                          framebuffer-width
                          framebuffer-height
                          scale
                          background)
        (values logical-width logical-height scale framebuffer-width framebuffer-height)))))

(defun save-screenshot (path
                        width
                        height)
  "Save the back buffer as a PNG.

PATH: output file.
WIDTH: framebuffer width in pixels.
HEIGHT: framebuffer height in pixels."
  (ensure-directories-exist path)
  (write-rgb-png path
                 width
                 height
                 (gl:read-pixels 0 0 width height :rgb :unsigned-byte)))

(defun dispatch-sdl-events (app
                            window
                            event
                            wait-milliseconds)
  "Read every pending SDL event (waiting up to WAIT-MILLISECONDS for the first) and hand the
translated events to the application.

APP: the application.
WINDOW: the SDL window.
EVENT: a reusable SDL event record.
WAIT-MILLISECONDS: 0 to poll, positive to wait."
  (let ((available (if (plusp wait-milliseconds)
                       (sdl2-ffi.functions:sdl-wait-event-timeout event wait-milliseconds)
                       (sdl2-ffi.functions:sdl-poll-event event))))
    (loop while (/= 0 available)
          do (dolist (translated (translate-sdl-event event window))
               (if (eq translated :redraw)
                   (app:app-request-redraw app)
                   (app:app-handle-event app translated)))
             (setf available (sdl2-ffi.functions:sdl-poll-event event)))))

(defconstant +bytes-per-mebibyte+ 1048576d0
  "Bytes in one MiB, as a double for conversions.")

(defstruct (session (:constructor make-session (&key app
                                                    window
                                                    renderer
                                                    telemetry
                                                    phases
                                                    demo
                                                    exit-after-demo
                                                    screenshot
                                                    steps
                                                    begun)))
  "The state of one run of the window loop.

APP: the application.
WINDOW: the SDL window.
RENDERER: the OpenGL renderer.
TELEMETRY: the observability hub.
PHASES: startup phase durations measured by CALL-WITH-GL-WINDOW.
DEMO: true while running the scripted demonstration.
EXIT-AFTER-DEMO: true to quit once the demonstration has finished.
SCREENSHOT: file to save the final demonstration frame to; NIL once saved or when not wanted.
STEPS: demonstration steps that have not run yet.
BEGUN: monotonic time at which the program started, in nanoseconds.
FRAMES: frames presented so far.
SETTLED: frames presented since the demonstration steps ran out.
FIRST-FRAME-LOGGED: true once the app.startup entry has been written.
TEXT-INPUT-ACTIVE: whether SDL text input is enabled at the moment.
TITLE: the window title set last.
CPU-START: monotonic time at which the CPU work of the current frame began."
  app
  window
  renderer
  telemetry
  phases
  demo
  exit-after-demo
  screenshot
  steps
  begun
  (frames 0)
  (settled 0)
  (first-frame-logged nil)
  (text-input-active nil)
  (title nil)
  (cpu-start 0))

(defun demo-running-p (session)
  "Return true while the demonstration still needs frames of its own: its steps have not run
out, it has not drawn its first frame, it has not settled, or its screenshot is still to be
taken. Its steps advance with the frames drawn, so a window that receives no event (a hidden or
covered one) must not stop them.

SESSION: the session."
  (and (session-demo session)
       (or (session-steps session)
           (not (session-first-frame-logged session))
           (< (session-settled session) +screenshot-settle-frames+)
           (session-screenshot session))
       t))

(defun wait-milliseconds (session)
  "Return how long the loop may sleep while waiting for an event: not at all while a redraw is
pending or the demonstration is running, a short time while the live overlay needs refreshing,
a longer one otherwise.

SESSION: the session."
  (let ((app (session-app session)))
    (cond ((or (app:app-needs-redraw app) (demo-running-p session)) 0)
          ((app:app-overlay-visible app) +overlay-idle-wait-milliseconds+)
          (t +idle-wait-milliseconds+))))

(defun run-due-demo-step (session)
  "Run the next demonstration step when its turn has come.

SESSION: the session."
  (when (and (session-steps session)
             (zerop (mod (session-frames session) +demo-frames-per-step+)))
    (funcall (pop (session-steps session)))
    (app:app-request-redraw (session-app session))))

(defun sync-text-input (session)
  "Enable or disable SDL text input to match what the application is waiting for.

SESSION: the session."
  (let ((wanted (and (app:app-wants-text-input-p (session-app session)) t)))
    (unless (eq wanted (session-text-input-active session))
      (if wanted
          (sdl2:start-text-input)
          (sdl2:stop-text-input))
      (setf (session-text-input-active session) wanted))))

(defun sync-window-title (session)
  "Set the window title when the application's title changed.

SESSION: the session."
  (let ((title (app:app-title (session-app session))))
    (unless (or (equal title (session-title session))
                (null (session-window session)))
      (sdl2:set-window-title (session-window session) title)
      (setf (session-title session) title))))

(defun frame-due-p (session)
  "Return true when a frame must be drawn: the application asked for a redraw, or the
demonstration is running and needs its frames.

SESSION: the session."
  (or (app:app-needs-redraw (session-app session))
      (demo-running-p session)))

(defun log-first-frame (session
                        presented)
  "Write the app.startup entry, once, when the first frame has been presented.

SESSION: the session.
PRESENTED: monotonic time at which the frame was presented, in nanoseconds."
  (unless (session-first-frame-logged session)
    (setf (session-first-frame-logged session) t)
    (let ((phases (session-phases session)))
      (obs:record-operation-result (session-telemetry session)
                                   "app.startup"
                                   (- presented (session-begun session))
                                   :info
                                   :window-ms (getf phases :window-ms)
                                   :context-ms (getf phases :context-ms)
                                   :renderer-ms (getf phases :renderer-ms)
                                   :vsync (getf phases :vsync)
                                   :result :success))))

(defun screenshot-due-p (session)
  "Return true when the demonstration has settled and a screenshot is still wanted.

SESSION: the session."
  (and (session-screenshot session)
       (session-demo session)
       (null (session-steps session))
       (>= (session-settled session) +screenshot-settle-frames+)))

(defun save-demo-screenshot (session
                             framebuffer-width
                             framebuffer-height)
  "Save the frame just drawn as the demonstration screenshot.

SESSION: the session.
FRAMEBUFFER-WIDTH: drawable width in pixels.
FRAMEBUFFER-HEIGHT: drawable height in pixels."
  (save-screenshot (session-screenshot session)
                   framebuffer-width
                   framebuffer-height)
  (obs:log-event (obs:telemetry-logger (session-telemetry session))
                 :info
                 "app.screenshot"
                 :path (session-screenshot session))
  (setf (session-screenshot session) nil))

(defun draw-frame (session)
  "Render the application into the window, present it and record the frame.

SESSION: the session."
  (let* ((app (session-app session))
         (telemetry (session-telemetry session)))
    (multiple-value-bind (logical-width
                          logical-height
                          scale
                          framebuffer-width
                          framebuffer-height)
        (present-frame (session-window session)
                       (session-renderer session)
                       (progn (app:app-render app)
                              (app:app-draw-list app))
                       (scene:theme-canvas-background (app:app-theme app)))
      (declare (ignore logical-width logical-height scale))
      (when (screenshot-due-p session)
        (save-demo-screenshot session
                              framebuffer-width
                              framebuffer-height))
      (let ((submitted (sbcl:monotonic-nanoseconds)))
        (sdl2:gl-swap-window (session-window session))
        (let ((presented (sbcl:monotonic-nanoseconds)))
          (obs:telemetry-record-frame telemetry
                                      (- submitted (session-cpu-start session))
                                      (- presented submitted)
                                      presented)
          (log-first-frame session presented))))))

(defun note-frame-presented (session)
  "Count the frame, settle the demonstration, and throttle when vertical sync is off.

SESSION: the session."
  (incf (session-frames session))
  (when (and (session-demo session)
             (null (session-steps session)))
    (incf (session-settled session))
    (when (and (null (session-screenshot session))
               (session-exit-after-demo session)
               (>= (session-settled session) +screenshot-settle-frames+))
      (setf (app:app-quit-requested (session-app session)) t)))
  (unless (getf (session-phases session) :vsync)
    (sleep +no-vsync-sleep-seconds+)))

(defun step-session (session
                     draw)
  "Do what one pass of the window loop does once the pending events have been handled: run the
demonstration step that is due, bring text input and the window title up to date, draw a frame
when one is due, and do the periodic telemetry work.

SESSION: the session.
DRAW: function of the session that draws and presents a frame (DRAW-FRAME in the program)."
  (setf (session-cpu-start session) (sbcl:monotonic-nanoseconds))
  (run-due-demo-step session)
  (sync-text-input session)
  (sync-window-title session)
  (when (frame-due-p session)
    (funcall draw session)
    (note-frame-presented session))
  (obs:telemetry-tick (session-telemetry session)))

(defun run-event-loop (session)
  "Run the window loop until the application asks to quit: handle the events, then step the
session.

SESSION: the session."
  (let ((app (session-app session)))
    (sdl2:with-sdl-event (event)
      (loop until (app:app-quit-requested app)
            do (dispatch-sdl-events app
                                    (session-window session)
                                    event
                                    (wait-milliseconds session))
               (step-session session #'draw-frame)))))

(defun run-session (session
                    config)
  "Prepare the draw list and the application, run the loop, and save the demonstration
drawing at the end.

SESSION: the session.
CONFIG: the configuration, which may name a file to open at startup."
  (let* ((app (session-app session))
         (list (app:app-draw-list app)))
    (setf (scene:draw-list-white-u list) (atlas-white-uv)
          (scene:draw-list-white-v list) (atlas-white-uv))
    (app:app-handle-event app (resize-event-for (session-window session)))
    (when (base:config-initial-file config)
      (app:load-drawing-into-app app (base:config-initial-file config)))
    (run-event-loop session)
    (when (session-demo session)
      (app:save-app-drawing app (app:demo-save-path app)))))

(defun run-application (config
                        &key demo
                             exit-after-demo
                             screenshot)
  "Open the window and run until the user quits. Everything measured is logged.

CONFIG: the application configuration.
DEMO: when true, run the scripted demonstration drawing.
EXIT-AFTER-DEMO: when true, quit when the demonstration has finished.
SCREENSHOT: file name to save the last demonstration frame to, or NIL."
  (let* ((begun (sbcl:monotonic-nanoseconds))
         (telemetry (obs:make-telemetry config))
         (logger (obs:telemetry-logger telemetry))
         (width (base:config-window-width config))
         (height (base:config-window-height config)))
    (obs:log-event logger
                   :info
                   "app.start"
                   :lisp (sbcl:lisp-version-string)
                   :heap-capacity-mib (/ (sbcl:heap-capacity-bytes) +bytes-per-mebibyte+)
                   :demo demo)
    (unwind-protect
         (handler-case
             (call-with-gl-window config
                                  width
                                  height
                                  "Borba CAD"
                                  (lambda (window renderer phases)
                                    (let ((app (app:make-app config
                                                             :telemetry telemetry
                                                             :width (float width 1d0)
                                                             :height (float height 1d0))))
                                      (run-session (make-session :app app
                                                                 :window window
                                                                 :renderer renderer
                                                                 :telemetry telemetry
                                                                 :phases phases
                                                                 :demo demo
                                                                 :exit-after-demo exit-after-demo
                                                                 :screenshot screenshot
                                                                 :steps (and demo (app:demo-steps app))
                                                                 :begun begun)
                                                   config))))
           (error (condition)
             (obs:log-condition logger
                                :error
                                "app.crash"
                                condition)
             (error condition)))
      (obs:log-event logger
                     :info
                     "app.shutdown"
                     :uptime-s (/ (- (sbcl:monotonic-nanoseconds) begun)
                                  (float sbcl:+nanoseconds-per-second+ 1d0)))
      (obs:close-telemetry telemetry))))

;;; Frame benchmark with the real renderer

(defstruct (frame-benchmark-result (:constructor make-frame-benchmark-result (frames
                                                                              cpu-ms
                                                                              gpu-wait-ms
                                                                              swap-ms
                                                                              interval-ms
                                                                              gl-renderer
                                                                              swap-interval)))
  "Frame timings measured with the real window and GPU.

FRAMES: frames measured.
CPU-MS: sample summary of CPU time per frame (build, submit), in milliseconds.
GPU-WAIT-MS: sample summary of the time glFinish waited for the GPU after the submit, which is
the GPU work that the CPU work did not already cover.
SWAP-MS: sample summary of the buffer swap (includes waiting for vertical sync).
INTERVAL-MS: sample summary of the time between presented frames.
GL-RENDERER: the OpenGL renderer string reported by the driver.
SWAP-INTERVAL: the swap interval in effect, as reported by SDL after it was requested."
  frames cpu-ms gpu-wait-ms swap-ms interval-ms gl-renderer swap-interval)

(defstruct (frame-samples (:constructor make-frame-samples ()))
  "The timings collected while a windowed benchmark runs.

CPU: sample set of the CPU time per frame.
GPU-WAIT: sample set of the time glFinish waited.
SWAP: sample set of the buffer swap time.
INTERVAL: sample set of the time between presented frames.
PREVIOUS: monotonic time at which the previous frame was presented, or NIL."
  (cpu (obs:make-sample-set))
  (gpu-wait (obs:make-sample-set))
  (swap (obs:make-sample-set))
  (interval (obs:make-sample-set))
  (previous nil))

(defun record-frame-timings (samples
                             started
                             submitted
                             drained
                             presented)
  "Add the phases of one frame to the samples.

SAMPLES: the frame samples.
STARTED: monotonic time at which the frame began, in nanoseconds.
SUBMITTED: time at which the draw commands had been submitted.
DRAINED: time at which glFinish returned.
PRESENTED: time at which the buffers had been swapped."
  (flet ((add (set duration)
           (obs:sample-set-add set (obs:nanoseconds->milliseconds duration))))
    (add (frame-samples-cpu samples) (- submitted started))
    (add (frame-samples-gpu-wait samples) (- drained submitted))
    (add (frame-samples-swap samples) (- presented drained))
    (when (frame-samples-previous samples)
      (add (frame-samples-interval samples) (- presented (frame-samples-previous samples))))))

(defun time-benchmark-frame (window
                             renderer
                             app
                             index
                             samples
                             recording)
  "Render one frame of the benchmark, timing its phases, and keep the timings when RECORDING.

WINDOW: the SDL window.
RENDERER: the renderer.
APP: the application being driven.
INDEX: frame number, which selects the input.
SAMPLES: the frame samples.
RECORDING: true to keep the timings, false while warming up."
  (let ((started (sbcl:monotonic-nanoseconds)))
    (app:apply-benchmark-input app index)
    (present-frame window
                   renderer
                   (progn (app:app-render app)
                          (app:app-draw-list app))
                   (scene:theme-canvas-background (app:app-theme app)))
    (let ((submitted (sbcl:monotonic-nanoseconds)))
      ;; Draining the pipeline exposes the GPU time; it also serializes CPU and GPU, so the
      ;; sum of both is an upper bound of a real frame's cost.
      (gl:finish)
      (let ((drained (sbcl:monotonic-nanoseconds)))
        (sdl2:gl-swap-window window)
        (let ((presented (sbcl:monotonic-nanoseconds)))
          (when recording
            (record-frame-timings samples
                                  started
                                  submitted
                                  drained
                                  presented))
          (setf (frame-samples-previous samples) presented))))))

(defun run-frame-benchmark (drawing
                            &key (frames 600)
                                 (warmup 30)
                                 (width 1440)
                                 (height 900)
                                 (swap-interval 1))
  "Render FRAMES frames of DRAWING with the shared benchmark input (a zoom oscillating about
the canvas center and a moving pointer), and return the measured CPU, swap and interval
times. Needs a display.

DRAWING: the drawing to show.
FRAMES: frames to measure, after the warm-up.
WARMUP: frames rendered first and not measured.
WIDTH: window width in logical pixels.
HEIGHT: window height in logical pixels.
SWAP-INTERVAL: 1 to wait for the display refresh at each swap, 0 to swap at once."
  (let ((config (base:default-config))
        (samples (make-frame-samples))
        (gl-renderer nil)
        (effective-interval nil))
    (call-with-gl-window config
                         width
                         height
                         "Borba CAD benchmark"
                         (lambda (window renderer phases)
                           (declare (ignore phases))
                           (let* ((app (app:make-app config
                                                     :drawing drawing
                                                     :width (float width 1d0)
                                                     :height (float height 1d0)))
                                  (list (app:app-draw-list app)))
                             (setf gl-renderer (gl:get-string :renderer)
                                   effective-interval (sdl2:gl-get-swap-interval))
                             (setf (scene:draw-list-white-u list) (atlas-white-uv)
                                   (scene:draw-list-white-v list) (atlas-white-uv))
                             (app:app-handle-event app (resize-event-for window))
                             (app:operation-zoom-extents app)
                             (dotimes (index (+ warmup frames))
                               (time-benchmark-frame window
                                                     renderer
                                                     app
                                                     index
                                                     samples
                                                     (>= index warmup)))))
                         :swap-interval swap-interval)
    (make-frame-benchmark-result frames
                                 (obs:summarize-samples (frame-samples-cpu samples))
                                 (obs:summarize-samples (frame-samples-gpu-wait samples))
                                 (obs:summarize-samples (frame-samples-swap samples))
                                 (obs:summarize-samples (frame-samples-interval samples))
                                 gl-renderer
                                 effective-interval)))

;;; Entry point

(defun debug-mode-p ()
  "Return true when BORBA_CAD_DEBUG is set to 1, so unexpected errors reach the debugger."
  (equal "1" (sbcl:environment-variable "BORBA_CAD_DEBUG")))

(defun main ()
  "Program entry point: parse the command line and environment, then run the application.
Exit status: 0 on a normal exit, 2 for a configuration problem, 1 for an unexpected error."
  (let ((arguments (sbcl:command-line-arguments)))
    (flet ((run ()
             (multiple-value-bind (config remaining)
                 (base:config-with-arguments (base:config-with-environment (base:default-config)) arguments)
               (let ((options (parse-platform-options remaining)))
                 (cond ((platform-options-help options)
                        (write-string *usage*)
                        (terpri))
                       ((not (display-available-p))
                        (error 'base:configuration-error
                               :reason (format nil "no graphical session found (WAYLAND_DISPLAY and DISPLAY ~
                                                    are unset); run inside a desktop session")))
                       (t (run-application config
                                           :demo (platform-options-demo options)
                                           :exit-after-demo (platform-options-exit-after-demo options)
                                           :screenshot (platform-options-screenshot options))))))))
      (handler-case (if (debug-mode-p)
                        (run)
                        (handler-bind ((base:configuration-error
                                         (lambda (condition)
                                           (format *error-output* "borba-cad: ~a~%~%~a~%" condition *usage*)
                                           (finish-output *error-output*)
                                           (sb-ext:exit :code 2 :abort t))))
                          (run)))
        (base:borba-cad-error (condition)
          (format *error-output* "borba-cad: ~a~%" condition)
          (finish-output *error-output*)
          (sb-ext:exit :code 1 :abort t))
        (error (condition)
          (format *error-output* "borba-cad: unexpected error: ~a~%" condition)
          (finish-output *error-output*)
          (sb-ext:exit :code 1 :abort t)))
      (finish-output)
      (sb-ext:exit :code 0))))

(defun executable-main ()
  "Entry point of the saved executable: MAIN, with the debugger disabled unless
BORBA_CAD_DEBUG=1, so that an unexpected condition ends the process instead of waiting for a
terminal that may not exist."
  (unless (debug-mode-p)
    (sb-ext:disable-debugger))
  (main))
