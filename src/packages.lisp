(defpackage #:graphite
  (:use #:common-lisp))

(defpackage #:graphite.color
  (:use #:common-lisp)
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
  (:use #:common-lisp #:graphite.utils)

  (:export :renderer
   :fill-enabled
           :fill-color
   :stroke-enabled
           :stroke-color
   :blend-pixel))

(defpackage #:graphite.renderer.bitmap
  (:use #:common-lisp #:graphite.renderer #:graphite.color #:graphite.utils)
  (:export :bitmap-renderer
   :make-bitmap-renderer))


(defpackage #:graphite.renderer.cairo
  (:use #:common-lisp #:graphite.renderer #:graphite.utils)
  (:local-nicknames (:c2 :cl-cairo2)))
