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
         (graph (make-graph `((:nodes ,(mapcar #'pak-to-node packages)
                               :node-attributes ())))))
    (is-true graph)
    (is (= (length (cl-dot:nodes-of graph)) 1))
    (is (typep (car (cl-dot:nodes-of graph)) 'cl-dot:node))
    (cl-dot:dot-graph graph "flat-packages.png" :format :png)))

(test gen-package-graph--flat-packages-with-dependencies
  (let ((graph (make-graph `((:nodes (,(pak-to-node (spak:make-pak :name "bar"))))
                             (:nodes (,(pak-to-node (spak:make-pak :name "foo"
                                                                   :depends-on-pkgs '("bar")))))))))
    (is-true graph)
    (is (= (length (cl-dot:nodes-of graph)) 2))
    (is (= (length (cl-dot:edges-of graph)) 1))
    (cl-dot:dot-graph graph "flat-packages-with-dependencies.png" :format :png)))

(test gen-package-graph--flat-packages-with-dependencies--exclude-edge
  (let* ((packages `(,(spak:make-pak :name "bar")
                     ,(spak:make-pak :name "foo"
                                     :depends-on-pkgs '("bar"))))
         (graph (make-graph `((:nodes ,(mapcar #'pak-to-node packages)))
                            :exclude-connections-to '("bar"))))
    (is-true graph)
    (is (= (length (cl-dot:nodes-of graph)) 2))
    (is (= (length (cl-dot:edges-of graph)) 0))))

(test gen-package-graph--flat-packages-replace-node-names
  (let* ((packages `(,(spak:make-pak :name "foo.bar.baz")))
         (graph (make-graph `((:nodes ,(mapcar #'pak-to-node packages)))
                            :replace-node-names '(("foo.bar." . "")))))
    (is (= (length (cl-dot:nodes-of graph)) 1))
    (is (equal (getf (cl-dot:attributes-of (car (cl-dot:nodes-of graph)))
                     :label)
               "baz"))))

(test gen-package-graph--clustered
  (let* ((node-sets `((:name "fooc"
                       :cluster-attributes (:bgcolor :green)
                       :nodes (,(pak-to-node (spak:make-pak :name "foo")))
                       :node-attributes ())
                      (:name "barc"
                       :cluster-attributes (:bgcolor :red)
                       :nodes (,(pak-to-node (spak:make-pak :name "bar")))
                       :node-attributes ())))
         (graph (make-graph node-sets :cluster t)))
    (is-true graph)
    (is (= (length (cl-dot:nodes-of graph)) 2))
    (is (typep (car (cl-dot:nodes-of graph)) 'cl-dot:node))
    (cl-dot:dot-graph graph "clustered.png" :format :png)))

(test gen-package-graph--clustered-with-dependencies
  (let* ((node-sets `((:name "fooc"
                       :cluster-attributes (:bgcolor :green)
                       :nodes (,(pak-to-node (spak:make-pak :name "bar"))))
                      (:name "barc"
                       :cluster-attributes (:bgcolor :red)
                       :nodes (,(pak-to-node (spak:make-pak :name "foo"
                                                            :depends-on-pkgs '("bar")))))))
         (graph (make-graph node-sets :cluster t)))
    (is-true graph)
    (is (= (length (cl-dot:nodes-of graph)) 2))
    (is (= (length (cl-dot:edges-of graph)) 1))
    (cl-dot:dot-graph graph "clustered-with-dependencies.png" :format :png)))

(test gen-package-graph--clustered-with-edges-between-clusters
  "Tests that when there are package dependencies between clusters,
the clusters are connected by edges."
  (let* ((node-sets `((:name "fooc"
                       :cluster-attributes (:bgcolor :green)
                       :nodes (,(pak-to-node (spak:make-pak :name "bar")))
                       :node-attributes (:fillcolor :red))
                      (:name "barc"
                       :cluster-attributes (:bgcolor :red)
                       :nodes (,(pak-to-node (spak:make-pak :name "foo"
                                                            :depends-on-pkgs '("bar"))))
                       :node-attributes (:fillcolor :blue :style :filled))))
         (graph (make-graph node-sets :cluster t :cluster-edges t)))
    (is-true graph)
    (is (= (length (cl-dot:nodes-of graph)) 2))
    (is (= (length (cl-dot:edges-of graph)) 1))
    (let ((grapg-str
            (with-output-to-string (s)
              (cl-dot:print-graph graph :stream s))))
      (print grapg-str)
      (is (search
           "\"2\" -> \"1\"[weight=\"5\",lhead=\"cluster_fooc\",ltail=\"cluster_barc\"];"
           grapg-str))
      (is (search
           "bgcolor=\"GREEN\";
    \"1\" [label=\"bar\",fillcolor=\"RED\"];
  }" grapg-str))
      (is (search
           "bgcolor=\"RED\";
    \"2\" [label=\"foo\",fillcolor=\"BLUE\",style=filled];
  }" grapg-str)))
    (cl-dot:dot-graph graph "clustered-with-edges-between-clusters.png" :format :png)))
