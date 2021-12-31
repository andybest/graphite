(in-package :render)

(defclass renderer () ())

(defgeneric set-blend-mode (renderer mode))
(defgeneric size (renderer))

(defgeneric point (renderer x y color)
  (:documentation "Draw a point at the specified coordinates"))

(defgeneric rect (renderer x y width height color))
