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
