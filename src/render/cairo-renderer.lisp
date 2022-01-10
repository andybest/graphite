(in-package #:graphite.renderer.cairo)

(defclass cairo-renderer (graphite.renderer:renderer)
  ((context :accessor cr-context
            :initarg :context)
   (matrix-stack :accessor cr-matrix-stack
                 :initform (make-array 0 :adjustable t :fill-pointer t))))

(defun make-pdf-renderer (output-path width height)
    (make-instance 'cairo-renderer
                   :context (c2:create-pdf-context output-path width height)))

;;; Internal utilities

(defun update-stroke-color (cr)
  (with-aref (r g b a) (stroke-color cr)
             (c2:set-source-rgba r g b a (cr-context cr))))

(defun update-fill-color (cr)
  (with-aref (r g b a) (fill-color cr)
             (c2:set-source-rgba r g b a (cr-context cr))))

(defun do-fill-stroke (cr)
  (cond
    ((and (fill-enabled cr)
         (stroke-enabled cr)) (progn
                                (update-fill-color cr)
                                (c2:fill-preserve (cr-context cr))
                                (update-stroke-color cr)
                                (c2:stroke (cr-context cr))))
    ((fill-enabled cr) (progn
                         (update-fill-color cr)
                         (c2:fill-path (cr-context cr))))
    ((stroke-enabled cr) (progn
                           (update-stroke-color cr)
                           (c2:stroke (cr-context cr))))))


;;; Renderer attributes

(defmethod set-line-cap ((cr cairo-renderer) cap-type)
  (c2:set-line-cap cap-type (cr-context cr)))

(defmethod set-line-join ((cr cairo-renderer) join-type)
  (c2:set-line-join join-type (cr-context cr)))

(defmethod set-blend-mode ((cr cairo-renderer) mode)
  (c2:set-operator (ecase mode
                     (:no-blend :source)
                     (:blend :over)
                     (:add :add
                          (cr-context cr)))))

(defmethod size ((cr cairo-renderer))
  (vector (c2:width (cr-context cr))
          (c2:height (cr-context cr))))

(defmethod push-state ((cr cairo-renderer))
  (c2:save (cr-context cr)))

(defmethod pop-state ((cr cairo-renderer))
  (c2:restore (cr-context cr)))


;;; 2D Primitives

(defmethod point ((cr cairo-renderer) x y)
  (when (stroke-enabled cr)
    (update-fill-color cr)
    (c2:rectangle x y 1 1 (cr-context cr))
    (c2:fill-path (cr-context cr))))

(defmethod line ((cr cairo-renderer) x1 y1 x2 y2)
  (when (stroke-enabled cr)
    (push-state cr)
    (update-stroke-color cr)
    (c2:move-to x1 y1 (cr-context cr))
    (c2:line-to x2 y2 (cr-context cr))
    (c2:stroke (cr-context cr))
    (pop-state cr)))

(defmethod rect ((cr cairo-renderer) x y width height)
  (push-state cr)
  (c2:rectangle x y width height (cr-context cr))
  (do-fill-stroke cr)
  (pop-state cr))

(defmethod ellipse ((cr cairo-renderer) x y width height)
  (when (> width 0)
    (push-state cr)
    (push-matrix cr)
    (translate cr x y)
    (scale cr 1 (/ height width))
    (translate cr (- x) (- y))
    (translate cr (* width 0.5) (* height 0.5))
    (c2:arc x y width 0 (* pi 2) (cr-context cr))
    (pop-matrix cr)
    (do-fill-stroke cr)))

(defmethod ellipse-c ((cr cairo-renderer) x y width height)
  (when (> width 0)
    (push-state cr)
    (push-matrix cr)
    (translate cr x y)
    (scale cr 1 (/ height width))
    (translate cr (- x) (- y))
    (c2:arc x y width 0 (* pi 2) (cr-context cr))
    (pop-matrix cr)
    (do-fill-stroke cr)
    (pop-state cr)))


;;; Paths

(defmethod begin-path ((cr cairo-renderer))
  (c2:new-path (cr-context cr)))

(defmethod close-path ((cr cairo-renderer))
  (c2:close-path (cr-context cr)))

(defmethod draw-path ((cr cairo-renderer))
  (do-fill-stroke cr))

(defmethod move-to ((cr cairo-renderer) x y)
  (c2:move-to x y (cr-context cr)))

(defmethod line-to ((cr cairo-renderer) x y)
  (c2:line-to x y (cr-context cr)))

(defmethod curve-to ((cr cairo-renderer) x1 y1 x2 y2 x3 y3)
  (c2:curve-to x1 y1 x2 y2 x3 y3 (cr-context cr)))

(defmethod arc ((cr cairo-renderer) x y radius angle1 angle2)
  (c2:arc x y radius angle1 angle2 (cr-context cr)))


;;; Matrix operations

(defmethod push-matrix ((cr cairo-renderer))
  (vector-push-extend (c2:get-trans-matrix (cr-context cr)) (cr-matrix-stack cr)))

(defmethod pop-matrix ((cr cairo-renderer))
  (c2:set-trans-matrix (vector-pop (cr-matrix-stack cr)) (cr-context cr)))

(defmethod translate ((cr cairo-renderer) tx ty)
  (c2:translate tx ty (cr-context cr)))

(defmethod scale ((cr cairo-renderer) sx sy)
  (c2:scale sx sy (cr-context cr)))

(defmethod rotate ((cr cairo-renderer) angle)
  (c2:rotate angle (cr-context cr)))
