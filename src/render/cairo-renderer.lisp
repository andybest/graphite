(in-package :renderer.cairo)

(defclass cairo-renderer (renderer:renderer)
  ((context :accessor cr-context
            :initarg :context)
   (matrix-stack :accessor cr-matrix-stack
                 :initform (make-array 0 :adjustable t :fill-pointer t))))

(defun make-pdf-renderer (output-path width height)
    (make-instance 'cairo-renderer
                   :context (cl-cairo2:create-pdf-context output-path width height)))

(defun update-stroke-color (cr)
  (with-aref (r g b a) (stroke-color cr)
             (cl-cairo2:set-source-rgba r g b a (cr-context cr))))

(defun update-fill-color (cr)
  (with-aref (r g b a) (fill-color cr)
             (cl-cairo2:set-source-rgba r g b a (cr-context cr))))

(defun do-fill-stroke (cr)
  (cond
    ((and (fill-enabled cr)
         (stroke-enabled cr)) (progn
                                (update-fill-color cr)
                                (cl-cairo2:fill-preserve (cr-context cr))
                                (update-stroke-color cr)
                                (cl-cairo2:stroke (cr-context cr))))
    ((fill-enabled cr) (progn
                         (update-fill-color cr)
                         (cl-cairo2:fill-path (cr-context cr))))
    ((stroke-enabled cr) (progn
                           (update-stroke-color cr)
                           (cl-cairo2:stroke (cr-context cr))))))

(defmethod set-blend-mode ((cr cairo-renderer) mode)
  (cl-cairo2:set-operator (ecase mode
                            (:no-blend :source)
                            (:blend :over)
                            (:add :add))
                          (cr-context cr)))

(defmethod size ((cr cairo-renderer))
  (vector (cl-cairo2:width (cr-context cr))
          (cl-cairo2:height (cr-context cr))))

(defmethod point ((cr cairo-renderer) x y)
  (update-fill-color cr)
  (cl-cairo2:rectangle x y 1 1 (cr-context cr))
  (cl-cairo2:fill-path (cr-context cr)))

(defmethod line ((cr cairo-renderer) x1 y1 x2 y2)
  (update-stroke-color cr)
  (cl-cairo2:move-to x1 y1 (cr-context cr))
  (cl-cairo2:line-to x2 y2 (cr-context cr))
  (cl-cairo2:stroke (cr-context cr)))

(defmethod rect ((cr cairo-renderer) x y width height)
  (cl-cairo2:rectangle x y width height (cr-context cr))
  (do-fill-stroke cr))


;; Matrix operations

(defmethod push-matrix ((cr cairo-renderer))
  (vector-push-extend (cl-cairo2:get-trans-matrix (cr-context cr)) (cr-matrix-stack cr)))

(defmethod pop-matrix ((cr cairo-renderer))
  (cl-cairo2:set-trans-matrix (vector-pop (cr-matrix-stack cr)) (cr-context cr)))

(defmethod translate ((cr cairo-renderer) tx ty)
  (cl-cairo2:translate tx ty (cr-context cr)))

(defmethod scale ((cr cairo-renderer) sx sy)
  (cl-cairo2:scale sx sy (cr-context cr)))

(defmethod rotate ((cr cairo-renderer) angle)
  (cl-cairo2:rotate angle (cr-context cr)))
