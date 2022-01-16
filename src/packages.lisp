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
  (:export :with-aref
           :string->hash
           :iteration-hash
           :do-until-pred))

(defpackage #:graphite.math
  (:use #:common-lisp))

(defpackage #:graphite.renderer
  (:use #:common-lisp #:graphite.utils)

  (:export
   :renderer
   :fill-enabled
   :fill-color
   :stroke-enabled
   :stroke-color
   :renderer-finalize
   :set-line-cap
   :set-line-join
   :set-blend-mode
   :size
   :push-state
   :pop-state
   :point
   :line
   :rect
   :ellipse
   :ellipse-c
   :begin-path
   :close-path
   :draw-path
   :move-to
   :line-to
   :curve-to
   :arc
   :translate
   :scale
   :rotate
   :push-matrix
   :pop-matrix
   :point-v
   :line-v
   :rect-v
   :ellipse-v
   :ellipse-c-v
   :move-to-v
   :line-to-v
   :curve-to-v
   :arc-v))

(defpackage #:graphite.renderer.bitmap
  (:use #:common-lisp #:graphite.renderer #:graphite.color #:graphite.utils)
  (:export :bitmap-renderer
   :make-bitmap-renderer))


(defpackage #:graphite.renderer.cairo
  (:use #:common-lisp #:graphite.utils)
  (:local-nicknames (:c2 :cl-cairo2)
                    (:r :graphite.renderer)))

(uiop:define-package #:graphite.rng
  (:use #:common-lisp #:graphite.utils #:pcg)
  (:export :make-opensimplex-2d
           :make-opensimplex-3d
           :make-opensimplex-4d
           :sample-noise
           :random-gaussian
           :make-rng)
  (:reexport :pcg))
  
(uiop:define-package #:graphite
  (:use #:cl
        #:graphite.color
        #:graphite.utils
        #:graphite.renderer)
  (:import-from #:graphite.renderer.cairo
                :make-pdf-renderer
                :make-png-renderer)
  (:reexport :graphite.color
             :graphite.utils
             :graphite.renderer)

  (:export :initial-state-seed
           :initial-state-seed-fixnum
           :draw-to-file
           :render-iterations))
