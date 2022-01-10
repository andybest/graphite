(in-package #:graphite.renderer.bitmap)

(defclass bitmap-renderer (graphite.renderer:renderer)
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
                 :image (opticl:make-single-float-rgb-image height width)))

(defun br-get-pixel (br x y)
  "Get a pixel from the renderer's graphics buffer"
  (opticl:pixel (br-image br) y x))

(defun br-blend-pixel (br x y color)
  "Blends an RGBA pixel into the renderer's RGB graphics buffer according to the current blend mode"
    (multiple-value-bind (r g b) (opticl:pixel (br-image br) y x)
      (setf (opticl:pixel (br-image br) y x)
            (graphite.utils:with-aref (r g b) (graphite.renderer:blend-pixel (br-blend-mode br) color (vector r g b))
              (values-list (list r g b))))))


(defmethod set-blend-mode ((br bitmap-renderer) mode)
  (setf (br-blend-mode br) mode))

(defmethod point ((br bitmap-renderer) x y)
   (br-blend-pixel br x y (graphite.color:rgba-to-byte (graphite.renderer:stroke-color br))))

(defun save-png (br path)
  "Save the renderer's graphics buffer to a PNG file at the specified path"
  (let ((image-8bit-rgb (opticl:convert-image-to-rgb (br-image br))))
    (opticl:write-png-file path (br-image br))))
