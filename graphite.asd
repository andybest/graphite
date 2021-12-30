(defsystem "graphite"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (#:alexandria #:cmu-infix #:cl-cairo2 #:cl-gd #:veq)
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "init")
                 (:file "utils")
                 (:file "render/renderer")
                 (:file "render/bitmap-renderer"))))
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
