(in-package :graphite)

(defstruct initial-state
  (seed (:type string)))

(defun draw-to-file (output-path renderer-type width height seed draw-func)
  (let ((renderer (make-renderer renderer-type output-path width height))
        (state (make-initial-state :seed seed)))
    (funcall draw-func renderer state)
    (renderer-finalize renderer)))

(defun make-renderer (renderer-type output-path width height)
  (ecase renderer-type
    (:pdf (make-pdf-renderer output-path width height))
    (:png (make-png-renderer output-path width height))))

(defun renderer-type-extension (renderer-type)
  "Returns the file extension for the given RENDERER-TYPE."
  (ecase renderer-type
    (:pdf "pdf")
    (:png "png")))

(defun graphite-render-dir (&optional subdir)
  (let* ((base-path (uiop:getenv-absolute-directory "GRAPHITE_RENDER_DIR"))
         (full-path (if (null subdir)
                        base-path
                        (merge-pathnames base-path (format nil "/~a" subdir)))))

    (uiop:ensure-pathname full-path :ensure-directories-exist t)))

(defun file-path-for-iteration (iteration seed sketch-name renderer-type width height)
  (let* ((base-path (graphite-render-dir (string-downcase (format nil "~a" sketch-name))))
         (filename (format nil "~a_i~d_~ax~a_~a.~a"
                           (string-downcase sketch-name)
                           iteration width height seed
                           (renderer-type-extension renderer-type))))
    (pathname (format nil "~a/~a" base-path filename))))

(defun render-iteration (iteration sketch-name renderer-type width height draw-func)
  (let* ((seed (iteration-hash iteration :base-string (format nil "~a-" sketch-name)))
         (path (file-path-for-iteration iteration seed sketch-name renderer-type width height)))
    (uiop:ensure-pathname path :ensure-directories-exist t)
    (draw-to-file path renderer-type width height seed draw-func)
    (print (format nil "Iteration ~d complete" iteration))))

(defun render-iterations (iterations sketch-name renderer-type width height draw-func &key parallel)
  (if parallel
      (progn
        (setf lparallel:*kernel* (lparallel:make-kernel (serapeum:count-cpus) :name (format nil "~a render kernel" sketch-name)))
        (lparallel:pdotimes (i iterations)
          (render-iteration i sketch-name renderer-type width height draw-func)))
      (dotimes (i iterations)
        (render-iteration i sketch-name renderer-type width height draw-func)))
  (print (format nil "Done rendering ~d iterations of ~a" iterations sketch-name)))
