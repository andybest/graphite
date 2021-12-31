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

(defpackage #:renderer
  (:use #:common-lisp))

(defpackage #:renderer.bitmap
  (:use #:common-lisp #:color #:renderer #:utils))
