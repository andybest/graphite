(in-package #:graphite.datastructures)

;;; Grid

(defun make-grid (width height &key (element-type 'single-float) (initial-element 0.0))
  "Make a 2 dimensional array with dimensions WIDTHxHEIGHT"
  (make-array `(,width ,height) :element-type element-type :initial-element initial-element))

(declaim (inline grid-width))
(defun grid-width (grid)
  "Returns the width of GRID"
  (array-dimension grid 0))

(declaim (inline grid-height))
(defun grid-height (grid)
  "Returns the width of GRID"
  (array-dimension grid 1))

(declaim (ftype (function (simple-array fixnum fixnum) boolean) grid-in-bounds-p))
(defun grid-in-bounds-p (grid x y)
  "Returns T if the coordinates X,Y are in the bounds of GRID"
  (and (>= x 0) (< x (grid-width grid))
       (>= y 0) (< y (grid-height grid))))

(defun grid-coords (grid x y)
  "Returns a set of coordinates in GRID that correspond to the nearest grid point as represented by the
normalized coordinates X,Y which are in the range 0..1, or nil if the coordinates are out of bounds."
  (let ((gx (round (* x (grid-width grid))))
        (gy (round (* y (grid-height grid)))))
    (if (grid-in-bounds-p grid gx gy)
      (values gx gy)
      (values nil nil))))

(declaim (inline grid-sample))
(defun grid-sample (grid x y)
  "Returns the value in GRID at the normalized coordinates X,Y, or nil if out of bounds"
  (multiple-value-bind (gx gy) (grid-coords grid x y)
    (when (and gx gy)
      (aref grid gx gy))))

(defun grid-neighbors (grid x y)
  "Returns the coordinates of the neighbouring points in GRID to the coordinates X,Y, including X,Y"
  (remove nil (alexandria:map-product 'list
                                      (loop for gx from (alexandria:clamp (- x 1) 0 (grid-width grid))
                                               to (alexandria:clamp (+ x 1) 0 (grid-width grid)) collect gx)
                                      (loop for gy from (alexandria:clamp (- y 1) 0 (grid-height grid))
                                               to (alexandria:clamp (+ y 1) 0 (grid-height grid)) collect gy))))

(defun grid-neighbors-norm (grid x y)
  "Returns the coordinates of the neighbouring points in GRID to the coordinates X,Y, including X,Y,
where X,Y are normalized coordinates in the range 0..1"
  (when (and (>= x 0.0) (>= y 0.0) (<= x 1.0) (<= y 1.0))
    (multiple-value-bind (gx gy) (grid-coords grid x y)
      (when (and gx gy (grid-in-bounds-p grid gx gy))
        (grid-neighbors grid x y)))))
