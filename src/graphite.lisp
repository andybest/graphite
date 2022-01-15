(in-package :graphite)

(defstruct initial-state
  (seed (:type string)))

(defun draw-to-file (output-path renderer-type width height seed draw-func)
  (print "Rendering sketch")
  (let ((renderer (make-renderer renderer-type output-path width height))
        (state (make-initial-state :seed seed)))
    (funcall draw-func renderer state)
    (renderer-finalize renderer)
    (print "Rendering complete")))

(defun make-renderer (renderer-type output-path width height)
  (ecase renderer-type
    (:pdf (make-pdf-renderer output-path width height))))
