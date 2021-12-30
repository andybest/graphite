(in-package :render)

(defclass renderer () ())

(defgeneric set-blend-mode (renderer mode))
(defgeneric size (renderer))
(defgeneric point (renderer x y))
(defgeneric rect (renderer x y width height))


