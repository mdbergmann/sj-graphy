(defsystem "graphy"
  :version "0.0.1"
  :author "Manfred Bergmann"
  :description "Lisp tools for SQC"
  :depends-on ("alexandria"
               "binding-arrows"
               "str"
               "fset"
               "cl-dot"
               "cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "scan-packages")
                 (:file "gen-package-graph")
               )))
  :in-order-to ((test-op (test-op "graphy/tests"))))

(defsystem "graphy/tests"
  :author "Manfred Bergmann"
  :depends-on ("graphy"
               "fiveam"
               "cl-mock")
  :components ((:module "test"
                :components
                ((:file "all-test")
                 (:file "scan-packages-test")
                 (:file "gen-package-graph-test")
                 )))
  :description "Test system for graphy"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:graphy.tests))))


#|
TODO:

OK - remove packages that don't have files
=> - allow to exclude connections
- allow attributes to be defined

|#
