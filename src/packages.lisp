(defpackage #:graphite
  (:use #:common-lisp))

(defpackage #:color
  (:use #:common-lisp)
  (:export :rgb
           :rgba
           :hsl
           :hsla
           :hsv
           :hsva
           :rgba-to-byte))

(defpackage #:utils
  (:use #:common-lisp)
  (:export :with-aref))

(defpackage #:render
  (:use #:common-lisp))

(defpackage #:bitmap-renderer
  (:use #:common-lisp #:color #:render #:utils))
