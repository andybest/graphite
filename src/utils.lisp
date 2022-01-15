(in-package #:graphite.utils)

(defmacro with-aref ((&rest indices) array &body body)
  "Allows for destructuring of arrays (like destructuring-bind)"
  (let ((a (gensym)))
    `(let ((,a ,array))
       (symbol-macrolet
           ,(loop
               for n from 0
               for i in indices
               collect (list i `(aref ,a ,n)))
         ,@body))))

(defun string->hash (s &optional (hash-type :md5))
  "Calculate the HASH-TYPE digest of the string S and return a hex string of the resulting hash"
  (ironclad:byte-array-to-hex-string (ironclad:digest-sequence hash-type (ironclad:ascii-string-to-byte-array s))))

(defun iteration-hash (iter &key (base-string "iteration-") (hash-type :md5))
  "Creates a different hash string for a particular iteration number, provided by ITER, which is appended to :BASE-STRING.
Used for creating a different random number generator seed for each iteration of an artwork."
  (string->hash (format nil "~s~d" base-string iter) :hash-type hash-type))

(defmacro do-until-pred (binding pred &rest actions)
  "Continually execute ACTIONS until PRED is met. The result of ACTIONS will be bound to BINDING.
Example:
(do-until-pred x (> x 900)
  (let ((r (random 1000)))
    (print r)
    r))"
  `(let ((,binding nil))
     (loop
       (setf ,binding (progn ,@actions))
       (when ,pred (return ,binding)))))
