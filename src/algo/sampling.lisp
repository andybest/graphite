(in-package #:graphite.algo.sampling)

(declaim (optimize (speed 3) (space 0) (debug 0)))

(defstruct poisson-ctx
  (cell-size 0.0 :type single-float)
  (width 0.0 :type single-float)
  (height 0.0 :type single-float)
  (grid-width 0 :type fixnum)
  (grid-height 0 :type fixnum)
  (min-dist 0.0 :type single-float)
  (min-dist-squared 0.0 :type single-float)
  (grid nil :type (simple-array (or null fixnum)))
  (samples nil :type (vector rtg-math.types:vec2))
  (active nil :type list))

(defun make-poisson (dist width height)
  (let* ((cell-size (/ dist (sqrt 2.0)))
         (grid-width (+ 1 (ceiling (/ width cell-size))))
         (grid-height (+ 1 (ceiling (/ height cell-size)))))

    (make-poisson-ctx :cell-size cell-size
                      :width width
                      :height height
                      :grid-width grid-width
                      :grid-height grid-height
                      :min-dist dist
                      :min-dist-squared (* dist dist)
                      :grid (ds:make-grid grid-width
                                          grid-height
                                          :element-type '(or null fixnum)
                                          :initial-element nil)
                      :samples (make-array 0 :element-type 'rtg-math.types:vec2 :adjustable t :fill-pointer t))))

(defun poisson-insert-sample (poisson s)
  (let ((gx (floor (/ (v:x s) (poisson-ctx-cell-size poisson))))
        (gy (floor (/ (v:y s) (poisson-ctx-cell-size poisson)))))
      (setf (aref (poisson-ctx-grid poisson) gx gy) (vector-push-extend s (poisson-ctx-samples poisson)))))

(defun poisson-valid-sample-p (poisson s)
  (let* ((sx (v:x s))
         (sy (v:y s))
         (cell-size (poisson-ctx-cell-size poisson))
         (nx (floor (/ sx cell-size)))
         (ny (floor (/ sy cell-size))))
    (unless (or (< sx 0) (< sy 0) (>= sx (poisson-ctx-width poisson)) (>= sy (poisson-ctx-height poisson)))
      (not (loop for coord in (ds:grid-neighbors (poisson-ctx-grid poisson) nx ny) do
              (let* ((gx (car coord))
                     (gy (cadr coord))
                     (p-idx (aref (poisson-ctx-grid poisson) gx gy)))
                (when (and p-idx
                           (< (v2:distance-squared (aref (poisson-ctx-samples poisson) p-idx) s)
                              (poisson-ctx-min-dist-squared poisson)))
                  (return t))))))))
          
        

(defun poisson-gen-sample (s dist rng)
  "Generate a new sample within DIST and 2*DIST of the point S"
  (let ((radius (pcg-random rng dist (* dist 2)))
        (angle (pcg-random rng (* pi 2))))
    (v2:+ (v2:rotate (v2:make 0.0 radius) angle) s)))

(defun poisson-loop (poisson rejection-limit current rng)
  "Loop up to REJECTION-LIMIT number of times trying to place a new sample. Returns T if sample is sucessfully placed."
  (loop repeat rejection-limit do
    (let ((sample (poisson-gen-sample current (poisson-ctx-min-dist poisson) rng)))
      (when (poisson-valid-sample-p poisson sample)
        (return sample)))))

(defun poisson-disc (rng dist width height &key (rejection-limit 30))
  "Generates a set of points between 0-WIDTH and 0-HEIGHT, with a minimum distance
DIST between samples. :REJECTION-LIMIT provides the number of failed attempts to place
samples before the algorithm rejects a point.

Uses Robert Bridston's algorithm from the paper 'Fast Poisson Disk Sampling in Arbitrary
Dimensions'."
  (let ((poisson (make-poisson dist width height))
        (s (v2:make (pcg-random rng width) (pcg-random rng height))))
    (poisson-insert-sample poisson s)
    (setf (poisson-ctx-active poisson) (cons s '()))

    (loop while (not (null (poisson-ctx-active poisson))) do
      (let ((sample (poisson-loop poisson rejection-limit (car (poisson-ctx-active poisson)) rng)))
        (if sample
            (progn
              (poisson-insert-sample poisson sample)
              (setf (poisson-ctx-active poisson) (cons sample (poisson-ctx-active poisson))))
            (progn
              (setf (poisson-ctx-active poisson) (cdr (poisson-ctx-active poisson)))))))
    (poisson-ctx-samples poisson)))
