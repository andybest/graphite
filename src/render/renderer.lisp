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

(defgeneric set-blend-mode (renderer mode))
(defgeneric size (renderer))

(defgeneric point (renderer x y)
  (:documentation "Draw a point at the specified coordinates"))

(defgeneric rect (renderer x y width height)
  (:documentation "Draw a rectangle at the specified coordinates"))
