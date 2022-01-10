(in-package #:graphite.renderer)

(defclass renderer ()
  ((fill :accessor fill-enabled
         :initform t)
   (stroke :accessor stroke-enabled
           :initform t)
   (fill-color :accessor fill-color
               :initform (graphite.color:rgb 0 0 0))
   (stroke-color :accessor stroke-color
                 :initform (graphite.color:rgb 0 0 0))))

;;; Renderer attributes

(defgeneric set-line-cap (renderer cap-type)
  (:documentation "Sets the rendering style for line endings.
Possible options are:
- `:butt' - start/stop the line exactly at the end point
- `:square' - use a square line ending, with the center of the square being the start/end point
- `:round' - use a round line ending, with the center of the circle being the start/end point"))

(defgeneric set-line-join (renderer join-type)
  (:documentation "Sets the line join style.
Possible options are:
- `:miter' - use a sharp corner
- `:round' - use a round join, with the center of the circle at the joint point
- `:bevel' - use a cut-off join, where the line is cut off at half the line width from the join point"))

(defgeneric set-blend-mode (renderer mode)
  (:documentation "Sets the current blend mode.
Possible options are:
- `:no-blend' - overrite the target with the source color
- `:blend' - traditional alpha blending
- `:add' - additive blending: src + dest'"))

(defgeneric size (renderer)
  (:documentation "Returns the size of the canvas"))

(defgeneric push-state (renderer)
  (:documentation "Makes a copy of the current renderer state and saves it to an internal stack"))

(defgeneric pop-state (renderer)
  (:documentation "Restores the last saved state from the internal stack"))


;;; 2D Primitives

(defgeneric point (renderer x y)
  (:documentation "Draw a point at X,Y"))

(defgeneric line (renderer x1 y1 x2 y2)
  (:documentation "Draw a line between X1,Y1 and X2,Y2"))

(defgeneric rect (renderer x y width height)
  (:documentation "Draw a rectangle at X,Y with the given WIDTH and HEIGHT"))


(defgeneric ellipse (renderer x y width height)
  (:documentation "Draws an ellipse with the top left at X,Y with the given WIDTH and HEIGHT"))

(defgeneric ellipse-c (renderer x y width height)
  (:documentation "Draws an ellipse centered at X,Y with the given WIDTH and HEIGHT"))

;;; Paths

(defgeneric begin-path (renderer)
  (:documentation "Clears the current path and starts a new one"))

(defgeneric close-path (renderer)
  (:documentation "Adds a line segment to the path from the current point to the first point in the path"))

(defgeneric draw-path (renderer)
  (:documentation "Draws the current path to the canvas using the current stroke and fill settings"))

(defgeneric move-to (renderer x y)
  (:documentation "Begin a new sub-path"))

(defgeneric line-to (renderer x y)
  (:documentation "Adds a line to the current path from the current point to X,Y"))

(defgeneric curve-to (renderer x1 y1 x2 y2 x3 y3)
  (:documentation "Adds a Bezier spline from the current point to position X3,Y3, using X1,Y1 and X2,Y2 as control points"))

(defgeneric arc (renderer x y radius angle1 angle2)
  (:documentation "Adds an arc of the given RADIUS to the current path, centered at X,Y. Begins at ANGLE1 and increases clockwise to ANGLE2"))


;;; Matrix operations

(defgeneric translate (renderer tx ty)
  (:documentation "Translates the origin by TX and TY"))

(defgeneric scale (renderer sx sy)
  (:documentation "Applies a scaling operation to the current matrix by SX and SY"))

(defgeneric rotate (renderer angle)
  (:documentation "Applies a rotation operation to the current matrix by ANGLE, specified in radians"))

(defgeneric push-matrix (renderer)
  (:documentation "Saves the current transformation matrix onto the stack"))

(defgeneric pop-matrix (renderer)
  (:documentation "Restores the transformation matrix from the stack"))


;;; Vector operations

(defun point-v (renderer p)
  "Draw a point at the coordinates specified by the vector P"
  (with-aref (x y) p
    (point renderer x y)))

(defun line-v (renderer p1 p2)
  "Draw a point between the coordinates specified by vectors P1 and P2"
  (with-aref (x1 y1) p1
    (with-aref (x2 y2) p2
      (line renderer x1 y1 x2 y2))))

(defun rect-v (renderer origin size)
  "Draw a rectangle with the top left corner at the vector ORIGIN with SIZE"
  (rect renderer (v:x origin) (v:y origin) (v:x size) (v:y size)))

(defun ellipse-v (renderer origin size)
  "Draw an ellipse with the top left corner at the vector ORIGIN with SIZE"
  (ellipse renderer (v:x origin) (v:y origin) (v:x size) (v:y size)))

(defun ellipse-c-v (renderer origin size)
  "Draw an ellipse with the center at the vector ORIGIN with SIZE"
  (ellipse-c renderer (v:x origin) (v:y origin) (v:x size) (v:y size)))

(defun move-to-v (renderer p)
  "Begin a new sub-path at the vector P"
  (move-to renderer (v:x p) (v:y p)))

(defun line-to-v (renderer p)
  "Adds a line to the current path from the current point to the vector P"
  (line-to renderer (v:x p) (v:y p)))

(defun curve-to-v (renderer c1 c2 p)
  "Adds a Bezier spline from the current point to vector P, using vectors C1 and C2 as control points"
  (curve-to renderer (v:x c1) (v:y c1) (v:x c2) (v:y c2) (v:x p) (v:y p)))

(defun arc-v (renderer p radius angle1 angle2)
  "Adds an arc of the given RADIUS to the current path centered at the vector P. Begins at ANGLE1 and increases clockwise to ANGLE2"
  (arc renderer (v:x p) (v:y p) radius angle1 angle2))
