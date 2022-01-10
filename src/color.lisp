;;;; Utilities to create and convert colors in a variety of different color spaces

(in-package #:graphite.color)

(defmacro rgb (r g b)
  "RGB values normalised between 0 and 1"
  `(v! ,r ,g, b, 1.0))

(defmacro rgba (r g b a)
  "RGBA values normalised between 0 and 1"
  `(v! ,r ,g ,b ,a))

(defun hsl (h s l)
  "Hue, Saturation and Lightness, converted to RGB"
  (multiple-value-bind (r g b) (dufy:hsl-to-rgb h s l)
    (v! r g b 1.0)))

(defun hsla (h s l a)
  "Hue, Saturation, Lightness and Alpha, converted to RGB"
  (multiple-value-bind (r g b a) (dufy:hsl-to-rgb h s l)
    (v! r g b a)))

(defun hsv (h s v)
  "Hue, Saturation and Value, converted to RGB"
  (multiple-value-bind (r g b) (dufy:hsv-to-rgb h s v)
    (v! r g b 1.0)))

(defun hsva (h s v a)
  "Hue, Saturation, Value and Alpha, converted to RGB"
  (multiple-value-bind (r g b) (dufy:hsv-to-rgb h s v)
    (v! r g b a)))

(defun rgba-to-byte (c)
  "Converts floating point RGBA values to integer values normalised between 0-255"
  (map 'vector
       (lambda (channel) (floor (alexandria:clamp channel 0.0 255.0)))
       (*s c 255.0)))

(defun clamp (c)
  "Clamps the given color within the range of 0.0 to 1.0"
  (map 'vector (lambda (channel) (alexandria:clamp channel 0.0 1.0)) c))
