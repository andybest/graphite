(defpackage #:graphite
  (:use #:common-lisp))

(defpackage #:graphite.color
  (:use #:common-lisp #:rtg-math)
  (:export :rgb
           :rgba
           :hsl
           :hsla
           :hsv
           :hsva
           :rgba-to-byte))

(defpackage #:graphite.utils
  (:use #:common-lisp)
  (:export :with-aref))

(defpackage #:graphite.renderer
  (:use #:common-lisp #:rtg-math #:graphite.utils)
  (:export :renderer
           :fill-enabled
           :fill-color
           :stroke-enabled
           :stroke-color
           :blend-pixel))

(defpackage #:graphite.renderer.bitmap
  (:use #:common-lisp #:graphite.color)
  (:export :bitmap-renderer
           :make-bitmap-renderer))


(defpackage #:graphite.renderer.cairo
  (:use #:common-lisp #:graphite.renderer #:graphite.utils))
