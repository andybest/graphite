(defpackage #:graphite
  (:use #:common-lisp))

(defpackage #:utils
  (:use #:common-lisp)
  (:export :with-aref))

(defpackage #:render
  (:use #:common-lisp #:utils))
