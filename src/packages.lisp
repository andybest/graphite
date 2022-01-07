(defpackage #:graphite
  (:use #:common-lisp))

(defpackage #:color
  (:use #:common-lisp #:rtg-math)
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
  (:use #:common-lisp #:rtg-math)
  (:export :renderer
           :fill-enabled
           :fill-color
           :stroke-enabled
           :stroke-color
           :blend-pixel))

(defpackage #:renderer.bitmap
  (:use #:common-lisp #:color)
  (:export :bitmap-renderer
           :make-bitmap-renderer))


(defpackage #:renderer.cairo
  (:use #:common-lisp #:renderer #:utils))
