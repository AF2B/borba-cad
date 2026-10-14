(in-package #:borba-cad/platform-tests)

(def-suite events
  :description "Translation of SDL events into application events."
  :in all)

(in-suite events)

(defconstant +test-sdl-key-a+ 97)
(defconstant +test-sdl-key-z+ 122)

(defmacro with-sdl-event ((variable type) &body body)
  "Run BODY with VARIABLE bound to a fresh SDL event of TYPE that never enters SDL's queue,
and free it afterwards.

VARIABLE: symbol bound to the event.
TYPE: SDL event type keyword.
BODY: forms to run."
  `(let ((,variable (sdl2:new-event ,type)))
     (unwind-protect (progn ,@body)
       (sdl2:free-event ,variable))))

(test letters-digits-and-named-keys-map-to-keywords
  (is (eq :a (platform:keycode->key +test-sdl-key-a+)))
  (is (eq :z (platform:keycode->key +test-sdl-key-z+)))
  (is (eq :escape (platform:keycode->key 27)))
  (is (eq :enter (platform:keycode->key 13)))
  (is (eq :backspace (platform:keycode->key 8)))
  (is (eq :delete (platform:keycode->key 127)))
  (is (eq :tab (platform:keycode->key 9)))
  (is (eq :space (platform:keycode->key 32))))

(test arrow-home-end-and-function-keys-use-the-scancode-bit
  (flet ((scancode (number) (logior #x40000000 number)))
    (is (eq :right (platform:keycode->key (scancode 79))))
    (is (eq :left (platform:keycode->key (scancode 80))))
    (is (eq :down (platform:keycode->key (scancode 81))))
    (is (eq :up (platform:keycode->key (scancode 82))))
    (is (eq :home (platform:keycode->key (scancode 74))))
    (is (eq :end (platform:keycode->key (scancode 77))))
    (is (eq :f1 (platform:keycode->key (scancode 58))))
    (is (eq :f12 (platform:keycode->key (scancode 69))))
    (is (eq :enter (platform:keycode->key (scancode 88))))))

(test unknown-keys-are-ignored
  (is (null (platform:keycode->key 0)))
  (is (null (platform:keycode->key 1000)))
  (is (null (platform:keycode->key (logior #x40000000 200)))))

(test mouse-motion-becomes-a-pointer-move
  (with-sdl-event (event :mousemotion)
    (setf (plus-c:c-ref event sdl2-ffi:sdl-event :motion :x) 120
          (plus-c:c-ref event sdl2-ffi:sdl-event :motion :y) 45)
    (let ((translated (platform:translate-sdl-event event nil)))
      (is (= 1 (length translated)))
      (let ((pointer (first translated)))
        (is (typep pointer 'app:pointer-event))
        (is (eq :move (app:pointer-event-action pointer)))
        (is (null (app:pointer-event-button pointer)))
        (is (= 120d0 (app:pointer-event-x pointer)))
        (is (= 45d0 (app:pointer-event-y pointer)))))))

(test mouse-buttons-map-to-left-middle-right-and-others-are-ignored
  (flet ((translate (type button)
           (with-sdl-event (event type)
             (setf (plus-c:c-ref event sdl2-ffi:sdl-event :button :button) button
                   (plus-c:c-ref event sdl2-ffi:sdl-event :button :x) 10
                   (plus-c:c-ref event sdl2-ffi:sdl-event :button :y) 20)
             (platform:translate-sdl-event event nil))))
    (let ((down (first (translate :mousebuttondown 1)))
          (up (first (translate :mousebuttonup 3)))
          (middle (first (translate :mousebuttondown 2))))
      (is (and (eq :down (app:pointer-event-action down)) (eq :left (app:pointer-event-button down))))
      (is (and (eq :up (app:pointer-event-action up)) (eq :right (app:pointer-event-button up))))
      (is (eq :middle (app:pointer-event-button middle)))
      (is (= 10d0 (app:pointer-event-x down)))
      (is (= 20d0 (app:pointer-event-y down))))
    (is (null (translate :mousebuttondown 4)))
    (is (null (translate :mousebuttondown 5)))))

(test the-wheel-reports-notches-and-ignores-a-zero-step
  (with-sdl-event (event :mousewheel)
    (setf (plus-c:c-ref event sdl2-ffi:sdl-event :wheel :y) 2)
    (let ((wheel (first (platform:translate-sdl-event event nil))))
      (is (typep wheel 'app:wheel-event))
      (is (= 2d0 (app:wheel-event-delta wheel)))))
  (with-sdl-event (event :mousewheel)
    (setf (plus-c:c-ref event sdl2-ffi:sdl-event :wheel :y) 0)
    (is (null (platform:translate-sdl-event event nil)))))

(test key-events-carry-the-key-and-the-action
  (flet ((translate (type keycode repeat)
           (with-sdl-event (event type)
             (setf (plus-c:c-ref event sdl2-ffi:sdl-event :key :keysym :sym) keycode
                   (plus-c:c-ref event sdl2-ffi:sdl-event :key :repeat) repeat)
             (platform:translate-sdl-event event nil))))
    (let ((down (first (translate :keydown +test-sdl-key-a+ 0)))
          (up (first (translate :keyup +test-sdl-key-a+ 0))))
      (is (eq :a (app:key-event-key down)))
      (is (eq :down (app:key-event-action down)))
      (is (eq :up (app:key-event-action up))))
    (is (null (translate :keydown 0 0)) "an unknown key produces nothing")
    (is (null (translate :keydown +test-sdl-key-a+ 1)) "auto-repeat of a letter is dropped")
    (is (= 1 (length (translate :keydown 8 1))) "auto-repeat of backspace is kept")))

(test text-input-is-decoded-as-utf-8
  (with-sdl-event (event :textinput)
    (loop for byte in '(#xC3 #xA9 #xC3 #xA7 #x61 0)
          for index from 0
          do (setf (plus-c:c-ref event sdl2-ffi:sdl-event :text :text index)
                   (if (> byte 127) (- byte 256) byte)))
    (let ((text (first (platform:translate-sdl-event event nil))))
      (is (typep text 'app:text-event))
      (is (equal (list (code-char 233) (code-char 231) #\a)
                 (coerce (app:text-event-text text) 'list))))))

(test quit-becomes-a-quit-event-and-other-events-are-ignored
  (with-sdl-event (event :quit)
    (let ((translated (platform:translate-sdl-event event nil)))
      (is (= 1 (length translated)))
      (is (typep (first translated) 'app:quit-event))))
  (with-sdl-event (event :joyaxismotion)
    (is (null (platform:translate-sdl-event event nil)))))

(test a-dropped-file-becomes-a-file-drop-event-and-its-path-is-freed
  (with-sdl-event (event :dropfile)
    (let* ((text (format nil "/home/me/obras/Planta T~crrea.bcad" (code-char 233)))
           (octets (sb-ext:string-to-octets text :external-format :utf-8))
           (buffer (sdl2-ffi.functions:sdl-malloc (1+ (length octets)))))
      (dotimes (index (length octets))
        (setf (cffi:mem-aref buffer :unsigned-char index) (aref octets index)))
      (setf (cffi:mem-aref buffer :unsigned-char (length octets)) 0)
      (setf (plus-c:c-ref event sdl2-ffi:sdl-event :drop :file) buffer)
      (let ((translated (platform:translate-sdl-event event nil)))
        (is (= 1 (length translated)))
        (is (typep (first translated) 'app:file-drop-event))
        (is (string= text (app:file-drop-event-path (first translated))))))))

(test a-drop-without-a-path-is-ignored
  (with-sdl-event (event :dropfile)
    (setf (cffi:mem-ref (autowrap:ptr event) :pointer platform::+sdl-drop-file-offset+) (cffi:null-pointer))
    (is (null (platform:translate-sdl-event event nil)))))

(test page-up-and-page-down-are-keys
  (flet ((scancode (number) (logior #x40000000 number)))
    (is (eq :page-up (platform:keycode->key (scancode 75))))
    (is (eq :page-down (platform:keycode->key (scancode 78))))))

(test losing-focus-and-the-pointer-leaving-become-window-state-events
  (flet ((translate (kind)
           (with-sdl-event (event :windowevent)
             (setf (plus-c:c-ref event sdl2-ffi:sdl-event :window :event) kind)
             (platform:translate-sdl-event event nil))))
    (let ((lost (translate 13))
          (left (translate 11)))
      (is (= 1 (length lost)))
      (is (typep (first lost) 'app:window-state-event))
      (is (eq :focus-lost (app:window-state-event-kind (first lost))))
      (is (eq :pointer-left (app:window-state-event-kind (first (translate 11)))))
      (is (= 1 (length left)))
      (is (null (translate 12)) "gaining focus needs nothing")
      (is (null (translate 10)) "nor does the pointer entering"))))

