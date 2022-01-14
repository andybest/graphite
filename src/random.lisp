(in-package #:graphite.rng)

(defun make-opensimplex-2d (&key seed)
  (cricket:open-simplex2s-2d :seed seed))

(defun make-opensimplex-3d (&key seed)
  (cricket:open-simplex2s-3d :seed seed))

(defun make-opensimplex-4d (&key seed)
  (cricket:open-simplex2s-4d :seed seed))

(defun sample-noise (n x &optional (y 0d0) (z 0d0) (w 0d0))
  (coerce (cricket:sample n
                          (coerce x 'double-float)
                          (coerce y 'double-float)
                          (coerce z 'double-float)
                          (coerce w 'double-float))
          'single-float))
