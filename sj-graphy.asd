(defsystem "sj-graphy"
  :version "0.0.1"
  :author "Manfred Bergmann"
  :description "Graphviz dot file generator for Scala/Java projects"
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
  :in-order-to ((test-op (test-op "sj-graphy/tests"))))

(defsystem "sj-graphy/tests"
  :author "Manfred Bergmann"
  :depends-on ("sj-graphy"
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
                                                           '#:sj-graphy.tests))))


#|
TODO:

OK - remove packages that don't have files
OK - allow to exclude connections to a package
- make connections between clusters
- count connections between clusters
- allow attributes to be defined

|#
