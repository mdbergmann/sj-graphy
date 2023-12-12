(defpackage :sj-graphy.gen-package-graph
  (:use :cl)
  (:nicknames :genpak)
  (:import-from #:spak
                #:pak
                #:pak-name
                #:pak-depends-on-pkgs)
  (:shadowing-import-from #:cl-dot
                          #:graph-object-node
                          #:graph-object-cluster
                          #:graph-object-points-to)
  (:export #:make-graph
           #:node
           #:make-node
           #:pak-to-node)
  )

(in-package :sj-graphy.gen-package-graph)

(defstruct cluster name attributes)
(defstruct node name cluster depends-on node-attributes)

(defun pak-to-node (pak &optional (cluster nil))
  "Converts a `SPAK:PAK' to a `NODE' for use in `MAKE-GRAPH'."
  (make-node :name (pak-name pak)
             :cluster cluster
             :depends-on (pak-depends-on-pkgs pak)))

(defvar *current-cluster* nil "The current cluster object.
When `MAKE-GRAPH' is called with a `CLUSTER' argument, this variable
is used to keep track of the current cluster object.")

(defvar *all-nodes* nil "A list of all nodes/clusters.")
(defvar *replace-node-names* nil "A list of conses of the form (\"old-name\" . \"new-name\").")
(defvar *exclude-connections-to* nil "A list of package names that should not be connected, aka no edges be created.")
(defvar *cluster-edges* nil "Generate edges between clusters when there are inter-connecting package dependencies.")

(defun make-graph (node-sets &key
                               (cluster nil)
                               (cluster-edges nil)
                               (replace-node-names nil)
                               (exclude-connections-to nil)
                               (attributes nil))
  "Generates a `CL-DOT:GRAPH' from a list of `NODE' sets.
If `CLUSTER' is non-nil, the graph will be clustered by the clusters:
`NODE-SETS' in this case is a plist like this:
  (:name \"cluster1\"
   :color :red
   :nodes (node1 node2 ...))
where `:nodes' is a list of `NODE's.
If `REPLACE-NODE-NAMES' is non-nil, the names of the packages will be replaced (or cut)
based on a provided alist. The alist is a list of conses of the form (\"old-name\" . \"new-name\").
I.e. if a common  package prefix should be replaced with an empyt string one would provide:
(\"foo.bar.\" . \"\"), resulting in the package \"foo.bar.baz\" being named \"baz\" in the graph.
`EXCLUDE-CONNECTIONS-TO' is a list of package names that should not be connected, aka no edges be created."
  (setf *current-cluster* nil)

  (let ((*replace-node-names* replace-node-names)
        (*exclude-connections-to* exclude-connections-to)
        (*cluster-edges* cluster-edges))
    (if cluster
        (cl-dot:generate-graph-from-roots
         'packages
         (setf *all-nodes*
               (reduce (lambda (accu cluster)
                         (let* ((cluster-name (getf cluster :name nil))
                                (cluster-attributes (getf cluster :cluster-attributes nil))
                                (node-attributes (getf cluster :node-attributes nil))
                                (nodes (getf cluster :nodes nil))
                                (cluster-node (make-cluster
                                               :name cluster-name
                                               :attributes cluster-attributes)))
                           (append accu
                                   (cons
                                    cluster-node
                                    (mapcar (lambda (node)
                                              (setf (node-cluster node) cluster-node
                                                    (node-node-attributes node) node-attributes)
                                              node)
                                            nodes)))))
                       node-sets
                       :initial-value nil))
         (list* :compound t attributes))
        (cl-dot:generate-graph-from-roots
         'packages
         (setf *all-nodes*
               (reduce (lambda (accu node-set)
                         (let ((node-attributes
                                 (getf node-set :node-attributes nil))
                               (nodes
                                 (getf node-set :nodes nil)))
                           (append accu
                                   (mapcar (lambda (node)
                                             (setf (node-node-attributes node)
                                                   node-attributes)
                                             node)
                                           nodes))))
                       node-sets
                       :initial-value nil))
         attributes))))

(defmethod graph-object-node ((graph (eql 'packages)) (object cluster))
  (format t "node ~a~%" object)
  (setf *current-cluster*
        (make-instance 'cl-dot:cluster
                       :id (cluster-name object)
                       :attributes (list* :label (cluster-name object)
                                          (cluster-attributes object))))
  nil)

(defmethod graph-object-node ((graph (eql 'packages)) (object node))
  (format t "node ~a~%" object)
  (make-instance 'cl-dot:node
                 :attributes (list* :label (%maybe-replace-pkg-name (node-name object))
                                    (node-node-attributes object))))

(defun %maybe-replace-pkg-name (name)
  (if *replace-node-names*
      (let ((new-name name))
        (loop :for (old . new) :in *replace-node-names*
              :do (setf new-name (cl-ppcre:regex-replace-all old new-name new)))
        new-name)
      name))

(defmethod graph-object-points-to ((graph (eql 'packages)) (object node))
  (format t "points-to ~a~%" object)
  (let ((cluster (node-cluster object)))
    (mapcar (lambda (ref)
              (if (and *cluster-edges*
                       (not (eq cluster (node-cluster ref))))
                  (make-instance 'cl-dot:attributed
                                 :object ref
                                 :attributes `(:weight 5
                                               :lhead ,(format nil "cluster_~a"
                                                               (cluster-name (node-cluster ref)))
                                               :ltail ,(format nil "cluster_~a"
                                                               (cluster-name cluster))))
                  (make-instance 'cl-dot:attributed
                                 :object ref
                                 :attributes '(:weight 3))))
            (%find-all-referencing-nodes-of object))))

(defun %find-all-referencing-nodes-of (node)
  (let* ((node-deps (node-depends-on node))
         (references
           (loop :for node-dep :in node-deps
                 :for references = (%find-nodes-referencing node-dep)
                 :append references)))
    (loop :for ref :in references
          :unless (member (node-name ref) *exclude-connections-to* :test #'equal)
            :collect ref)))

(defun %find-nodes-referencing (node-name)
  (loop :for no :in *all-nodes*
        :when (and (typep no 'node)
                   (equal node-name (node-name no)))
          :collect no))

(defmethod graph-object-cluster ((graph (eql 'packages)) object)
  (format t "graph-object-cluster ~a~%" object)
  (typecase object
    (cluster nil)
    (node *current-cluster*)))
