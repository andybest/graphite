(in-package #:graphite.algo.flowfield)

(defun make (size-x size-y)
  (make-array `(,size-x ,size-y) :element-type 'rtg-math.types:vec2 :initial-element (v2:make 0.0 1.0)))

(defun width (f)
  "Returns the width of the flow field F."
  (array-dimension f 0))

(defun height (f)
  "Returns the height of the flow field F."
  (array-dimension f 1))

(defun get-at (f x y)
  "Get the vector at index X,Y from flow field F."
  (aref f x y))

(defun set-at (f x y newvec)
  "Set the vector at index X,Y in flow field F to NEWVEC."
  (setf (aref f x y) newvec))

(defun get-nearest (f x y)
  "Get the nearest vector in the flowfield to X,Y, where X and Y are normalized coordinates between 0 and 1"
  (let* ((width (width f))
         (height (height f))
         (xc (round (* width (alexandria:clamp x 0.0 1.0))))
         (yc (round (* height (alexandria:clamp y 0.0 1.0)))))
    (print (format nil "~d, ~d" xc yc))
    (aref f xc yc)))

(defun get-nearest-v (f v)
  "Get the nearest vector in the flowfield to the vector V, where the elements are normalized coordinates between 0 and 1"
  (get-nearest f (v:x v) (v:y v)))

(defun normalize (f)
  "Normalize all of the vectors in flow field F."
  (loop for x from 0 below (width f) do
    (loop for y from 0 below (height f) do
      (set-at f x y
              (v2:normalize (get-at f x y))))))

(defun visualize (f r &key (x 0.0) (y 0.0) (width 1.0) (height 1.0))
  "Visualise the flow field F with renderer R"
  (graphite.renderer:push-matrix r)
  (graphite.renderer:translate r x y)
  (graphite.renderer:scale r width height)

  (let* ((fw (width f))
         (fh (height f))
         (xspacing (/ width fw))
         (yspacing (/ height fh)))
    (loop for ix from 0 below width do
      (loop for iy from 0 below height do
        (let ((px (* xspacing ix))
              (py (* yspacing iy)))
          (graphite.renderer:ellipse-c r px py (* xspacing 0.1) (* xspacing 0.1))
          (graphite.renderer:line-v r
                                    (v2:make px py)
                                    (v2:+ (v2:make px py) (v2:*s (v2:normalize (get-at f ix iy)) (* xspacing 0.75))))))))

  (graphite.renderer:pop-matrix r))
