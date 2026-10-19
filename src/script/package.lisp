(defpackage #:borba-cad/doubles
  (:use)
  (:documentation "The functions of Common Lisp that give a single-float when they are given a whole number (SQRT 2
is 1.4142135), made to give a double-float, as the program counts. The user package takes these in place of those
of Common Lisp, so that what is typed in the console is as exact as the drawing.")
  (:export #:sqrt
           #:sin
           #:cos
           #:tan
           #:asin
           #:acos
           #:atan
           #:exp
           #:log
           #:float))

(defpackage #:borba-cad/script
  (:use #:cl)
  (:local-nicknames (#:base #:borba-cad/base)
                    (#:doubles #:borba-cad/doubles)
                    (#:i18n #:borba-cad/i18n)
                    (#:geo #:borba-cad/geometry)
                    (#:dom #:borba-cad/domain)
                    (#:persist #:borba-cad/persistence)
                    (#:sbcl #:borba-cad/sbcl-support))
  (:documentation "The language the program can be driven with: commands that draw and edit, written in Lisp
and run against a drawing; the context they run in, with the transaction that makes each command a single step
of undo; the evaluator, which reads, runs and reports; and the writer that turns a drawing back into the
commands that draw it.")
  (:export
   ;; Context
   #:script-context
   #:make-script-context
   #:script-context-drawing
   #:context-changes
   #:context-change-count
   #:context-rollback
   #:context-reset
   #:*maximum-changes*
   #:*context*
   ;; Evaluating
   #:script-result
   #:script-result-status
   #:script-result-values
   #:script-result-output
   #:script-result-message
   #:script-result-forms
   #:evaluate-source
   #:read-source-forms
   #:count-source-forms
   #:strip-comments
   #:+default-timeout-seconds+
   #:+script-timeout-seconds+
   ;; Writing a drawing, or a session, as commands
   #:drawing-script
   #:steps-script
   ;; Units, angles, colors
   #:mm
   #:cm
   #:m
   #:inch
   #:foot
   #:rad
   #:rgb
   ;; Layers
   #:layer
   #:with-layer
   #:layers
   ;; Drawing
   #:line
   #:rect
   #:rect-size
   #:circle
   #:arc
   #:ellipse
   #:polygon
   #:pline
   #:text
   #:dimension
   #:with-block
   #:make-block
   #:insert
   #:explode
   #:spread
   #:array-grid
   #:array-polar
   #:array-path
   #:region-union
   #:region-subtract
   #:region-intersect
   #:blocks
   #:hatch
   #:hatch-polygon
   #:radius-dimension
   #:diameter-dimension
   #:angle-dimension
   ;; Editing
   #:move
   #:copy
   #:rotate
   #:mirror
   #:scale
   #:erase
   #:edit
   #:redraw
   #:move-to-layer
   ;; Asking
   #:entities
   #:entity-count
   #:info
   #:extents
   ;; Selection and view
   #:select
   #:select-all
   #:deselect
   #:selected
   #:fit
   ;; The drawing's own settings
   #:display-unit
   #:grid-spacing
   ;; Talking
   #:say
   #:help))

(defpackage #:borba-cad/user
  (:use #:cl #:borba-cad/script)
  (:shadowing-import-from #:borba-cad/doubles
                          #:sqrt
                          #:sin
                          #:cos
                          #:tan
                          #:asin
                          #:acos
                          #:atan
                          #:exp
                          #:log
                          #:float)
  (:documentation "The package that what the user types in the console is read in: all of Common Lisp and the
commands of the program, and nothing else."))
