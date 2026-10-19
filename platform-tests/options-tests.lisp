(in-package #:borba-cad/platform-tests)

(def-suite options
  :description "The command-line options only the window program understands."
  :in all)

(in-suite options)

(test a-bare-argument-names-the-drawing-to-open
  (let ((options (platform:parse-platform-options '("plan.bcad"))))
    (is (string= "plan.bcad" (platform:platform-options-file options)))
    (is (not (platform:platform-options-demo options)))))

(test the-demonstration-options-are-understood-and-a-screenshot-file-is-not-a-drawing
  (let ((options (platform:parse-platform-options '("--demo" "--exit-after-demo" "--screenshot" "shot.png"))))
    (is (platform:platform-options-demo options))
    (is (platform:platform-options-exit-after-demo options))
    (is (string= "shot.png" (platform:platform-options-screenshot options)))
    (is (null (platform:platform-options-file options)) "the value of --screenshot is not a bare path")))

(test a-drawing-and-options-can-come-in-any-order
  (let ((options (platform:parse-platform-options '("--demo" "plan.bcad"))))
    (is (string= "plan.bcad" (platform:platform-options-file options)))
    (is (platform:platform-options-demo options))))

(test unknown-options-and-a-second-bare-argument-are-refused
  (signals base:configuration-error (platform:parse-platform-options '("--nonsense")))
  (signals base:configuration-error (platform:parse-platform-options '("one.bcad" "two.bcad")))
  (signals base:configuration-error (platform:parse-platform-options '("--screenshot"))))

(test help-is-recognized
  (is (platform:platform-options-help (platform:parse-platform-options '("--help"))))
  (is (platform:platform-options-help (platform:parse-platform-options '("-h")))))

(test the-program-knows-the-folder-it-runs-from
  (let ((directory (platform::executable-directory)))
    (is (pathnamep directory))
    (is (null (pathname-name directory)) "it is a directory pathname")
    (is (eq :directory (sbcl:file-kind directory)))))

(test an-installed-program-starts-from-its-own-folder-and-the-user-state-directory
  (let ((installed (platform::starting-configuration t))
        (source (platform::starting-configuration nil)))
    (is (equal (platform::executable-directory) (base:config-project-root installed)))
    (is (equal (base:user-state-directory) (base:config-state-directory installed)))
    (is (equal (merge-pathnames "log/" (base:user-state-directory)) (base:config-log-directory installed)))
    (is (equal (merge-pathnames "var/log/" (base:config-project-root source)) (base:config-log-directory source))
        "run from its source, the program keeps using var/ in the project")))

(test windows-needs-no-variable-to-have-a-graphical-session
  (is (platform:display-available-p :windows-p t) "a Windows machine has a window system, whatever the variables")
  #-win32
  (let ((wayland (sbcl:environment-variable "WAYLAND_DISPLAY"))
        (x11 (sbcl:environment-variable "DISPLAY")))
    (unwind-protect
         (progn
           (sb-posix:unsetenv "WAYLAND_DISPLAY")
           (sb-posix:unsetenv "DISPLAY")
           (is (platform:display-available-p :windows-p t))
           (is (not (platform:display-available-p :windows-p nil)) "on the other systems the variables say"))
      (when wayland
        (sb-posix:setenv "WAYLAND_DISPLAY" wayland 1))
      (when x11
        (sb-posix:setenv "DISPLAY" x11 1)))))

;;; The window adapts to the screen and the interface to the window

(test the-interface-is-drawn-as-designed-in-a-window-that-has-room
  (is (= 1d0 (platform::interface-scale-for 1100 640 1d0)))
  (is (= 1d0 (platform::interface-scale-for 1440 900 1d0)))
  (is (= 1d0 (platform::interface-scale-for 3840 2160 1d0)) "a big window does not enlarge the interface by itself"))

(test the-interface-is-drawn-smaller-in-a-window-smaller-than-its-layout
  (is (= 0.8d0 (platform::interface-scale-for 880 800 1d0)) "the narrow side decides")
  (is (< (abs (- (/ 480d0 640) (platform::interface-scale-for 1600 480 1d0))) 1d-9) "or the low one")
  (is (< (abs (- 0.75d0 (platform::interface-scale-for 825 480 1d0))) 1d-9))
  (is (= 0.6d0 (platform::interface-scale-for 300 200 1d0)) "never below the smallest"))

(test the-zoom-asked-for-is-kept-within-what-the-window-has-room-for
  (is (= 1.25d0 (platform::interface-scale-for 1920 1080 1.25d0)))
  (is (= 1.8d0 (platform::interface-scale-for 1980 1152 2d0)) "no more than the window allows")
  (is (= 0.8d0 (platform::interface-scale-for 880 640 1.5d0)) "a small window cannot be magnified")
  (is (< (abs (- 0.6d0 (platform::interface-scale-for 880 640 0.75d0))) 1d-9) "but it can be made smaller still"))

(test a-window-opens-no-bigger-than-the-screen-and-no-smaller-than-the-minimum
  (is (equal '(1440 900) (multiple-value-list (platform::fit-window-size 1440 900 3840 2160))) "room to spare")
  (is (equal '(1177 662) (multiple-value-list (platform::fit-window-size 1440 900 1280 720)))
      "a small screen: 92 per cent of what is usable")
  (is (equal '(1440 900) (multiple-value-list (platform::fit-window-size 1440 900 nil nil)))
      "without the size of the screen, as asked")
  (is (equal (list platform::+minimum-window-width+ platform::+minimum-window-height+)
             (multiple-value-list (platform::fit-window-size 100 100 300 300)))))

(test control-with-minus-plus-or-zero-are-the-keys-that-zoom
  (is (eq :minus (platform::zoom-keycode->key 45)))
  (is (eq :plus (platform::zoom-keycode->key 43)))
  (is (eq :plus (platform::zoom-keycode->key 61)) "the equals key, with the plus on it")
  (is (eq :zero (platform::zoom-keycode->key 48)))
  (is (eq :plus (platform::zoom-keycode->key (logior #x40000000 87))) "the plus of the keypad")
  (is (eq :minus (platform::zoom-keycode->key (logior #x40000000 86))))
  (is (null (platform::zoom-keycode->key 97)) "a letter is not a zoom key"))
