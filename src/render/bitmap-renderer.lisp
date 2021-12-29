(in-package :render)

(defclass bitmap-renderer (renderer)
  ((width :accessor br-width
          :initarg :width)
   (height :accessor br-height
           :initarg :height)
   (image :accessor br-image
          :initarg :image)))

(defun make-bitmap-renderer (width height)
  (make-instance 'bitmap-renderer
                 :width width
                 :height height
                 :image (opticl-core:make-32-bit-rgba-image height width)))
