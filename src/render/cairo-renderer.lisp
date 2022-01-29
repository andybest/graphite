(in-package #:graphite.renderer.cairo)

(defclass cairo-renderer (graphite.renderer:renderer)
  ((type :accessor cr-type
         :initarg :type)
   (output-path :accessor cr-output-path
                :initarg :output-path)
   (context :accessor cr-context
            :initarg :context)
   (matrix-stack :accessor cr-matrix-stack
                 :initform (make-array 0 :adjustable t :fill-pointer t))
   (stroke-width :accessor cr-stroke-width
                 :initform 1.0)
   (px-scale :accessor cr-px-scale
             :initform 1.0)))

(defun make-pdf-renderer (output-path width height)
    (make-instance 'cairo-renderer
                   :type :pdf
                   :output-path :output-path
                   :context (c2:create-pdf-context output-path width height)))

(defun make-svg-renderer (output-path width height)
  (make-instance 'cairo-renderer
                 :type :svg
                 :output-path :output-path
                 :context (c2:create-svg-context output-path width height)))

(defun make-png-renderer (output-path width height)
  (let* ((surface (c2:create-image-surface :rgb24 width height))
         (context (c2:create-context surface)))
    (c2:destroy surface)
    (make-instance 'cairo-renderer
                   :type :png
                   :output-path output-path
                   :context context)))

(defmethod r:renderer-finalize ((cr cairo-renderer))
  (case (cr-type cr)
    (:png (c2:surface-write-to-png (c2:get-target (cr-context cr)) (cr-output-path cr))))
  (c2:destroy (cr-context cr)))

;;; Internal utilities

(defun update-sizes (cr)
  "Update the stroke width and size of point based on the current scale"
  (let ((px-scale (c2:device-to-user-distance 1 1 (cr-context cr))))
    (c2:set-line-width (* (cr-stroke-width cr) px-scale) (cr-context cr))
    (setf (cr-px-scale cr) px-scale)))

(defun update-stroke-color (cr)
  (with-aref (r g b a) (r:stroke-color cr)
             (c2:set-source-rgba r g b a (cr-context cr))))

(defun update-fill-color (cr)
  (with-aref (r g b a) (r:fill-color cr)
             (c2:set-source-rgba r g b a (cr-context cr))))

(defun do-fill-stroke (cr)
  (cond
    ((and (r:fill-enabled cr)
          (r:stroke-enabled cr)) (progn
                                   (update-fill-color cr)
                                   (c2:fill-preserve (cr-context cr))
                                   (update-stroke-color cr)
                                   (c2:stroke (cr-context cr))))
    ((r:fill-enabled cr) (progn
                           (update-fill-color cr)
                           (c2:fill-path (cr-context cr))))
    ((r:stroke-enabled cr) (progn
                             (update-stroke-color cr)
                             (c2:stroke (cr-context cr))))))


;;; Renderer attributes

(defmethod r:set-line-cap ((cr cairo-renderer) cap-type)
  (c2:set-line-cap cap-type (cr-context cr)))

(defmethod r:set-line-join ((cr cairo-renderer) join-type)
  (c2:set-line-join join-type (cr-context cr)))

(defmethod r:set-blend-mode ((cr cairo-renderer) mode)
  (c2:set-operator (ecase mode
                     (:no-blend :source)
                     (:blend :over)
                     (:add :add
                      (cr-context cr)))))

(defmethod r:size ((cr cairo-renderer))
  (vector (c2:width (cr-context cr))
          (c2:height (cr-context cr))))

(defmethod r:push-state ((cr cairo-renderer))
  (c2:save (cr-context cr)))

(defmethod r:pop-state ((cr cairo-renderer))
  (c2:restore (cr-context cr)))


;;; 2D Primitives

(defmethod r:point ((cr cairo-renderer) x y)
  (when (r:stroke-enabled cr)
    (update-stroke-color cr)
    (let ((psize (cr-px-scale cr)))
      (c2:rectangle x y psize psize (cr-context cr)))
    (c2:fill-path (cr-context cr))))

(defmethod r:line ((cr cairo-renderer) x1 y1 x2 y2)
  (when (r:stroke-enabled cr)
    (r:push-state cr)
    (update-stroke-color cr)
    (c2:move-to x1 y1 (cr-context cr))
    (c2:line-to x2 y2 (cr-context cr))
    (c2:stroke (cr-context cr))
    (r:pop-state cr)))

(defmethod r:rect ((cr cairo-renderer) x y width height)
  (r:push-state cr)
  (c2:rectangle x y width height (cr-context cr))
  (do-fill-stroke cr)
  (r:pop-state cr))

(defmethod r:ellipse-c ((cr cairo-renderer) x y width height)
  (when (> width 0)
    (r:push-state cr)
    (r:push-matrix cr)
    (r:translate cr x y)
    (r:scale cr 1 (/ height width))
    (r:translate cr (- x) (- y))
    (c2:arc x y (* 0.5 width) 0 (* pi 2) (cr-context cr))
    (r:pop-matrix cr)
    (do-fill-stroke cr)
    (r:pop-state cr)))

(defmethod r:ellipse ((cr cairo-renderer) x y width height)
  (when (> width 0)
    (r:push-matrix cr)
    (r:translate cr (* width 0.25) (* height 0.25))
    (r:pop-matrix cr)))

;;; Paths

(defmethod r:begin-path ((cr cairo-renderer))
  (c2:new-path (cr-context cr)))

(defmethod r:close-path ((cr cairo-renderer))
  (c2:close-path (cr-context cr)))

(defmethod r:draw-path ((cr cairo-renderer))
  (do-fill-stroke cr))

(defmethod r:move-to ((cr cairo-renderer) x y)
  (c2:move-to x y (cr-context cr)))

(defmethod r:line-to ((cr cairo-renderer) x y)
  (c2:line-to x y (cr-context cr)))

(defmethod r:curve-to ((cr cairo-renderer) x1 y1 x2 y2 x3 y3)
  (c2:curve-to x1 y1 x2 y2 x3 y3 (cr-context cr)))

(defmethod r:arc ((cr cairo-renderer) x y radius angle1 angle2)
  (c2:arc x y radius angle1 angle2 (cr-context cr)))


;;; Matrix operations

(defmethod r:push-matrix ((cr cairo-renderer))
  (vector-push-extend (c2:get-trans-matrix (cr-context cr)) (cr-matrix-stack cr)))

(defmethod r:pop-matrix ((cr cairo-renderer))
  (c2:set-trans-matrix (vector-pop (cr-matrix-stack cr)) (cr-context cr))
  (update-sizes cr))

(defmethod r:translate ((cr cairo-renderer) tx ty)
  (c2:translate tx ty (cr-context cr)))

(defmethod r:scale ((cr cairo-renderer) sx sy)
  (c2:scale sx sy (cr-context cr))
  (update-sizes cr))

(defmethod r:rotate ((cr cairo-renderer) angle)
  (c2:rotate angle (cr-context cr)))
