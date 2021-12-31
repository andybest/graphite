(in-package :bitmap-renderer)

(defclass bitmap-renderer (renderer)
  ((width :accessor br-width
          :initarg :width)
   (height :accessor br-height
           :initarg :height)
   (image :accessor br-image
          :initarg :image)
   (blend-mode :accessor br-blend-mode
               :initform :blend)))

(defun make-bitmap-renderer (width height)
  (make-instance 'bitmap-renderer
                 :width width
                 :height height
                 :image (opticl:make-32-bit-rgb-image height width)))

(defun br-get-pixel (br x y)
  "Get a pixel from the renderer's graphics buffer"
  (opticl:pixel (br-image br) y x))

(defun br-blend-pixel (br x y color)
  "Blends an RGBA pixel into the renderer's RGB graphics buffer according to the current blend mode"
  (let ((dest (opticl:pixel (br-image br) y x)))
    (setf (opticl:pixel (br-image br) y x) (blend-pixel (br-blend-mode br) color dest))))

(defmethod set-blend-mode ((br bitmap-renderer) mode)
  (setf (br-blend-mode br) mode))

(defmethod point ((br bitmap-renderer) x y color)
   (br-blend-pixel br x y (color:rgba-to-byte color)))
