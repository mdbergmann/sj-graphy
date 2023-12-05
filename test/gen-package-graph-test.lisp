(defpackage :graphy.gen-package-graph-test
  (:use :cl :fiveam :graphy.gen-package-graph)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :graphy.gen-package-graph-test)

(def-suite gen-package-graph-tests
  :description "Tests for generating package graphs"
  :in graphy.tests:test-suite)

(in-suite gen-package-graph-tests)

(test gen-package-graph--flat-packages
  (let* ((packages `(,(spak:make-pak :name "foo")))
         (graph (make-graph packages)))
    (is-true graph)
    (is (= (length (cl-dot:nodes-of graph)) 1))
    (is (typep (car (cl-dot:nodes-of graph)) 'cl-dot:node))
    (cl-dot:dot-graph graph "flat-packages.png" :format :png)))

(test gen-package-graph--clustered
  (let* ((packages `((:name "fooc"
                      :color :green
                      :packages (,(spak:make-pak :name "foo")))
                     (:name "barc"
                      :color :red
                      :packages (,(spak:make-pak :name "bar")))))
         (graph (make-graph packages :cluster t)))
    (is-true graph)
    (is (= (length (cl-dot:nodes-of graph)) 2))
    (is (typep (car (cl-dot:nodes-of graph)) 'cl-dot:node))
    (cl-dot:dot-graph graph "clustered.png" :format :png)))
  

(run! 'gen-package-graph-tests)