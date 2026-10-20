(in-package #:borba-cad/app)

;;;; Parametric blocks in the application: a generator (DEFBLOCK, in the init file or the console) draws a block from
;;;; some numbers. Placing one asks for the numbers in a form and puts the block it draws in the drawing, where the
;;;; usual tool places it; the placement then shows the numbers in the inspector, and changing one draws the block
;;;; again, as one step of undo. The drawing keeps what each block was drawn from, but not the code, which a drawing
;;;; file never holds, so a drawing opened where the generator is not defined still shows its blocks and cannot draw
;;;; them again.

(defun block-generators (app)
  "Return the generators of parametric blocks that are defined.

APP: the application."
  (script:customizations-generators (app-customizations app)))

(defun find-block-generator-in (app
                                name)
  "Return the generator of that name, or NIL.

APP: the application.
NAME: its name, in any case."
  (script:find-block-generator (app-customizations app) name))

(defun generator-parameter-key (generator
                                parameter)
  "Return the key under which what was typed for a parameter is kept for the next time.

GENERATOR: the generator.
PARAMETER: the name of the parameter."
  (intern (string-upcase (format nil "BLOCK ~a ~a" (script:block-generator-name generator) parameter)) :keyword))

(defun parameter-field-key (parameter)
  "Return the key of the line of a form that asks for a parameter.

PARAMETER: the name of the parameter."
  (intern (string-upcase parameter) :keyword))

(defun generator-numbers (generator
                          values)
  "Return the numbers that a form of the parameters of a generator holds, signaling INVALID-INPUT when one is not a
number.

GENERATOR: the generator.
VALUES: the alist of the form."
  (loop for (name) in (script:block-generator-parameters generator)
        collect (base:parse-decimal (form-value values (parameter-field-key name)))))

(defun place-parametric-block (app
                               generator
                               values)
  "Draw the block that a form of the parameters of a generator asks for, put it in the drawing and make the tool that
places it the active one. Returns NIL when it was done, or a sentence that says why it could not be, so that the form
stays.

APP: the application.
GENERATOR: the generator.
VALUES: the alist of the form."
  (handler-case
      (let* ((numbers (generator-numbers generator values))
             (definition (let ((script:*customizations* (app-customizations app)))
                           (script:parametric-block (app-drawing app)
                                                    generator
                                                    numbers))))
        (remember-form-settings app
                                (loop for (name) in (script:block-generator-parameters generator)
                                      collect (cons (generator-parameter-key generator name)
                                                    (form-value values (parameter-field-key name)))))
        (start-insert-tool app (dom:block-definition-name definition))
        nil)
    (base:borba-cad-error (condition)
      (remove #\Newline (princ-to-string condition)))))

(defun ask-block-parameters (app
                             generator)
  "Ask the numbers that a generator draws its block from, and go on to the tool that places the block.

APP: the application.
GENERATOR: the generator."
  (let ((parameters (script:block-generator-parameters generator)))
    (if (null parameters)
        (place-parametric-block app
                                generator
                                '())
        (open-form-prompt app
                          (script:block-generator-name generator)
                          (if (plusp (length (script:block-generator-description generator)))
                              (script:block-generator-description generator)
                              (i18n:tr "The numbers the block is drawn from; the inspector changes them later."))
                          (loop for (name . default) in parameters
                                collect (list (parameter-field-key name)
                                              name
                                              (form-setting app
                                                            (generator-parameter-key generator name)
                                                            (base:format-decimal (or default
                                                                                     script:+default-parameter-value+)
                                                                                 0))))
                          (lambda (values) (place-parametric-block app
                                                                   generator
                                                                   values))))))

(defun operation-insert-parametric-block (app
                                          name)
  "Ask for the numbers of a parametric block and place it.

APP: the application.
NAME: the name of its generator."
  (let ((generator (find-block-generator-in app name)))
    (if generator
        (ask-block-parameters app generator)
        (set-message app
                     (i18n:trf "There is no parametric block called ~a." name)
                     :error))))

(defun operation-edit-parameter (app
                                 entity-id
                                 parameter
                                 value)
  "Give a parameter of a placed parametric block another number, which draws the block again, as one step of undo.

APP: the application.
ENTITY-ID: the id of the placement.
PARAMETER: the name of the parameter.
VALUE: its new number."
  (handler-case
      (let ((changes (let ((script:*customizations* (app-customizations app)))
                       (script:parameter-changes (app-drawing app)
                                                 entity-id
                                                 parameter
                                                 value))))
        (cond ((null changes)
               (set-message app (i18n:tr "The block is already drawn from that number.")))
              ((commit-changes app
                               changes
                               "entity.edit"
                               :context "parameter-edit")
               (set-message app (i18n:trf "The block was drawn again with ~a at ~a."
                                          parameter
                                          (base:format-decimal value 4))))))
    (base:borba-cad-error (condition)
      (report-error app
                    condition
                    "parameter-edit"))))
