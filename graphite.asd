(defsystem "graphite"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (#:alexandria #:cmu-infix #:cl-cairo2 #:dufy #:opticl #:rtg-math)
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "init")
                 (:file "color")
                 (:file "utils")
                 (:file "render/blend")
                 (:file "render/renderer")
                 (:file "render/bitmap-renderer")
                 (:file "render/cairo-renderer"))))
  :description ""
  :in-order-to ((test-op (test-op "graphite/tests"))))

(defsystem "graphite/tests"
  :author ""
  :license ""
  :depends-on ("graphite"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for graphite"
  :perform (test-op (op c) (symbol-call :rove :run c)))
