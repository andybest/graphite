(in-package :renderer)

(defclass renderer ()
  ((fill :accessor fill-enabled
         :initform t)
   (stroke :accessor stroke-enabled
           :initform t)
   (fill-color :accessor fill-color
               :initform (color:rgb 0 0 0))
   (stroke-color :accessor stroke-color
                 :initform (color:rgb 0 0 0))))

(defgeneric set-blend-mode (renderer mode)
  (:documentation "Sets the current blend mode"))

(defgeneric size (renderer)
  (:documentation "Returns the size of the canvas"))

(defgeneric point (renderer x y)
  (:documentation "Draw a point at the specified coordinates"))

(defgeneric line (renderer x1 y1 x2 y2)
  (:documentation "Draw a line between the specified coordinates"))

(defgeneric rect (renderer x y width height)
  (:documentation "Draw a rectangle at the specified coordinates"))


;; Matrix operations

(defgeneric translate (renderer tx ty)
  (:documentation "Translates the origin by TX and TY"))

(defgeneric scale (renderer sx sy)
  (:documentation "Applies a scaling operation to the current matrix by SX and SY"))

(defgeneric rotate (renderer angle)
  (:documentation "Applies a rotation operation to the current matrix by ANGLE, specified in radians"))

(defgeneric push-matrix (renderer)
  (:documentation "Saves the current transformation matrix onto the stack"))

(defgeneric pop-matrix (renderer)
  (:documentation "Restores the transformation matrix from the stack"))
