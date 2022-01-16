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

(defun random-gaussian (pcg)
  "Returns a random number generated with PCG in a gaussian distribution in the range of 0.0-1.0.
Uses the Box-Muller transform"
  (do-until-pred n (and (<= n 1.0) (>= n 0.0))
    (let ((u (do-until-pred u (> u 0) (pcg:pcg-random pcg 1.0)))
          (v (do-until-pred v (> v 0) (pcg:pcg-random pcg 1.0))))
      (+ (/ (* (sqrt (* -2.0 (log u)))
               (cos (* 2.0 pi v)))
            10.0)
         0.5))))

(defun make-rng (seed)
  (make-pcg :seed (coerce (sxhash seed) '(unsigned-byte 64))))
