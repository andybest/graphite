(in-package #:graphite.renderer)

(defclass renderer ()
  ((fill :accessor fill-enabled
         :initform t)
   (stroke :accessor stroke-enabled
           :initform t)
   (fill-color :accessor fill-color
               :initform (graphite.color:rgb 0 0 0))
   (stroke-color :accessor stroke-color
                 :initform (graphite.color:rgb 0 0 0))))

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


;; Vector operations

(defun point-v (renderer p)
  "Draw a point at the coordinates specified by the vector P"
  (with-aref (x y) p
    (point renderer x y)))

(defun line-v (renderer p1 p2)
  "Draw a point between the coordinates specified by vectors P1 and P2"
  (with-aref (x1 y1) p1
    (with-aref (x2 y2) p2
      (line renderer x1 y1 x2 y2))))
