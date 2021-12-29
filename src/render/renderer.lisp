(in-package :render)

(defclass renderer () ())

(defgeneric size (renderer))
(defgeneric point (renderer x y))
(defgeneric rect (renderer x y width height))


