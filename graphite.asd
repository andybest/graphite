(defsystem "graphite"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (#:alexandria #:cmu-infix #:cl-cairo2
                            #:dufy #:opticl
                            #:rtg-math #:cricket
                            #:cl-pcg #:ironclad
                            #:lparallel #:serapeum)

  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "graphite")
                 (:file "init")
                 (:file "color")
                 (:file "utils")
                 (:file "math")
                 (:file "datastructures")
                 (:file "random")
                 (:file "render/blend")
                 (:file "render/renderer")
                 (:file "render/bitmap-renderer")
                 (:file "render/cairo-renderer")
                 (:file "algo/flowfield")
                 (:file "algo/sampling"))))

  :description ""
  :in-order-to ((test-op (test-op "graphite/tests"))))

(defsystem "graphite/tests"
  :author ""
  :license ""
  :depends-on (#:graphite #:rove)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for graphite"
  :perform (test-op (op c) (symbol-call :rove :run c)))
