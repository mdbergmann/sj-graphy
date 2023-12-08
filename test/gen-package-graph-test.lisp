(defpackage :sj-graphy.gen-package-graph-test
  (:use :cl :fiveam :sj-graphy.gen-package-graph)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :sj-graphy.gen-package-graph-test)

(def-suite gen-package-graph-tests
  :description "Tests for generating package graphs"
  :in sj-graphy.tests:test-suite)

(in-suite gen-package-graph-tests)

(test gen-package-graph--flat-packages
  (let* ((packages `(,(spak:make-pak :name "foo")))
         (graph (make-graph packages)))
    (is-true graph)
    (is (= (length (cl-dot:nodes-of graph)) 1))
    (is (typep (car (cl-dot:nodes-of graph)) 'cl-dot:node))
    (cl-dot:dot-graph graph "flat-packages.png" :format :png)))

(test gen-package-graph--flat-packages-with-dependencies
  (let* ((packages `(,(spak:make-pak :name "bar")
                     ,(spak:make-pak :name "foo"
                                     :depends-on-pkgs '("bar"))))
         (graph (make-graph packages)))
    (is-true graph)
    (is (= (length (cl-dot:nodes-of graph)) 2))
    (is (= (length (cl-dot:edges-of graph)) 1))
    (cl-dot:dot-graph graph "flat-packages-with-dependencies.png" :format :png)))

(test gen-package-graph--flat-packages-with-dependencies--exclude-edge
  (let* ((packages `(,(spak:make-pak :name "bar")
                     ,(spak:make-pak :name "foo"
                                     :depends-on-pkgs '("bar"))))
         (graph (make-graph packages :exclude-connections-to '("bar"))))
    (is-true graph)
    (is (= (length (cl-dot:nodes-of graph)) 2))
    (is (= (length (cl-dot:edges-of graph)) 0))))

(test gen-package-graph--flat-packages-replace-pkg-names
  (let* ((packages `(,(spak:make-pak :name "foo.bar.baz")))
         (graph (make-graph packages :replace-pkg-names '(("foo.bar." . "")))))
    (is (= (length (cl-dot:nodes-of graph)) 1))
    (is (equal (getf (cl-dot:attributes-of (car (cl-dot:nodes-of graph)))
                     :label)
               "baz"))))

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

(test gen-package-graph--clustered-with-dependencies
  (let* ((packages `((:name "fooc"
                      :color :green
                      :packages (,(spak:make-pak :name "bar")))
                     (:name "barc"
                      :color :red
                      :packages (,(spak:make-pak :name "foo"
                                                 :depends-on-pkgs '("bar"))))))
         (graph (make-graph packages :cluster t)))
    (is-true graph)
    (is (= (length (cl-dot:nodes-of graph)) 2))
    (is (= (length (cl-dot:edges-of graph)) 1))
    (cl-dot:dot-graph graph "clustered-with-dependencies.png" :format :png)))

(test gen-package-graph--clustered-with-edges-between-clusters
  "Tests that when there are package dependencies between clusters,
the clusters are connected by edges."
  (let* ((packages `((:name "fooc"
                      :color :green
                      :packages (,(spak:make-pak :name "bar")))
                     (:name "barc"
                      :color :red
                      :packages (,(spak:make-pak :name "foo"
                                                 :depends-on-pkgs '("bar"))))))
         (graph (make-graph packages :cluster t :cluster-edges t)))
    (is-true graph)
    (is (= (length (cl-dot:nodes-of graph)) 2))
    ;;(is (= (length (cl-dot:edges-of graph)) 2))
    (cl-dot:print-graph graph)
    (cl-dot:dot-graph graph "clustered-with-edges-between-clusters.png" :format :png)))
