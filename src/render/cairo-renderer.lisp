(in-package #:graphite.renderer.cairo)

(defclass cairo-renderer (graphite.renderer:renderer)
  ((context :accessor cr-context
            :initarg :context)
   (matrix-stack :accessor cr-matrix-stack
                 :initform (make-array 0 :adjustable t :fill-pointer t))))

(defun make-pdf-renderer (output-path width height)
    (make-instance 'cairo-renderer
                   :context (c2:create-pdf-context output-path width height)))

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

(defmethod set-blend-mode ((cr cairo-renderer) mode)
  (c2:set-operator (ecase mode
                     (:no-blend :source)
                     (:blend :over)
                     (:add :add
                          (cr-context cr)))))

(defmethod size ((cr cairo-renderer))
  (vector (c2:width (cr-context cr))
          (c2:height (cr-context cr))))

(defmethod point ((cr cairo-renderer) x y)
  (update-fill-color cr)
  (c2:rectangle x y 1 1 (cr-context cr))
  (c2:fill-path (cr-context cr)))

(defmethod line ((cr cairo-renderer) x1 y1 x2 y2)
  (update-stroke-color cr)
  (c2:move-to x1 y1 (cr-context cr))
  (c2:line-to x2 y2 (cr-context cr))
  (c2:stroke (cr-context cr)))

(defmethod rect ((cr cairo-renderer) x y width height)
  (c2:rectangle x y width height (cr-context cr))
  (do-fill-stroke cr))


;; Matrix operations

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
