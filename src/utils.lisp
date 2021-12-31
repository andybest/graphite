(in-package :utils)

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
