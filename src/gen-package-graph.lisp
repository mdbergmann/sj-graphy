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
  (:export #:make-graph)
  )

(in-package :sj-graphy.gen-package-graph)

(defstruct cluster name color)
(defstruct node name package cluster depends-on)

(defvar *current-cluster* nil "The current cluster object.
When `MAKE-GRAPH' is called with a `CLUSTER' argument, this variable
is used to keep track of the current cluster object.")

(defvar *all-nodes* nil "A list of all nodes/clusters.")
(defvar *replace-pkg-names* nil "A list of conses of the form (\"old-name\" . \"new-name\").")
(defvar *exclude-connections-to* nil "A list of package names that should not be connected, aka no edges be created.")
(defvar *cluster-edges* nil "Generate edges between clusters when there are inter-connecting package dependencies.")

(defun make-graph (packages &key
                              (cluster nil)
                              (cluster-edges nil)
                              (replace-pkg-names nil)
                              (exclude-connections-to nil))
  "Generates a `cl-dot:graph' from a list of `spak:pak's.
If `CLUSTER' is non-nil, the graph will be clustered by the clusters:
`PACKAGES' in this case is a plist like this:
  (:name \"cluster1\"
   :color :red
   :packages (package1 package2 ...))
where `:packages' is a list of `spak:pak's generated from `scan-packages'.
If `REPLACE-PKG-NAMES' is non-nil, the names of the packages will be replaced (or cut)
based on a provided alist. The alist is a list of conses of the form (\"old-name\" . \"new-name\").
I.e. if a common  package prefix should be replaced with an empyt string one would provide:
(\"foo.bar.\" . \"\"), resulting in the package \"foo.bar.baz\" being named \"baz\" in the graph.
`EXCLUDE-CONNECTIONS-TO' is a list of package names that should not be connected, aka no edges be created."
  (setf *current-cluster* nil)

  (let ((*replace-pkg-names* replace-pkg-names)
        (*exclude-connections-to* exclude-connections-to)
        (*cluster-edges* cluster-edges))
    (if cluster
        (cl-dot:generate-graph-from-roots
         'packages
         (setf *all-nodes*
               (reduce (lambda (accu cluster)
                         (let* ((cluster-node
                                 (make-cluster
                                  :name (getf cluster :name nil)
                                  :color (getf cluster :color nil)))
                                (nodes (mapcar (lambda (pak)
                                                 (make-node
                                                  :name (pak-name pak)
                                                  :package pak
                                                  :cluster cluster-node
                                                  :depends-on (pak-depends-on-pkgs pak)))
                                               (getf cluster :packages nil))))
                           (append accu (cons cluster-node nodes))))
                       packages
                       :initial-value nil))
         '(:compound t))
        (cl-dot:generate-graph-from-roots 'packages
                                          (setf *all-nodes*
                                                (mapcar (lambda (pak)
                                                          (make-node
                                                           :name (pak-name pak)
                                                           :package pak
                                                           :cluster nil
                                                           :depends-on (pak-depends-on-pkgs pak)))
                                                        packages))))))

(defmethod graph-object-node ((graph (eql 'packages)) (object cluster))
  (format t "node ~a~%" object)
  (setf *current-cluster*
        (make-instance 'cl-dot:cluster
                       :id (cluster-name object)
                       :attributes `(:label ,(cluster-name object)
                                     :bgcolor ,(cluster-color object))))
  nil)

(defmethod graph-object-node ((graph (eql 'packages)) (object node))
  (format t "node ~a~%" object)
  (make-instance 'cl-dot:node
                 :attributes `(:label ,(%maybe-replace-pkg-name (node-name object))
                               :shape :box)))

(defun %maybe-replace-pkg-name (name)
  (if *replace-pkg-names*
      (let ((new-name name))
        (loop :for (old . new) :in *replace-pkg-names*
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

(defmethod graph-object-points-to ((graph (eql 'packages)) (object cluster))
  (format t "cluster points-to ~a~%" object)
  ;; (let ((all-elements (coerce *all-nodes* 'vector)))
  ;;   ;; cluster indices
  ;;   (destructuring-bind (our-cluster-index . cluster-indices)
  ;;       (%find-indices-of-clusters object all-elements)
  ;;     (let* ((from-index (1+ our-cluster-index))
  ;;            (to-index (or (find-if (lambda (i) (> i our-cluster-index))
  ;;                                   cluster-indices)
  ;;                          (length all-elements)))
  ;;            (own-pak-seq (subseq all-elements from-index to-index)))
  ;;       (format t "our-cluster-index: ~a~%" our-cluster-index)
  ;;       (format t "from-index: ~a~%" from-index)
  ;;       (format t "to-index: ~a~%" to-index)
  ;;       (format t "cluster-indices: ~a~%" cluster-indices)
  ;;       (format t "own-pak-seq: ~a~%" own-pak-seq)
  ;;       (when own-pak-seq
  ;;         (let ((dependents
  ;;                 (loop :for pak :across own-pak-seq
  ;;                       :append (pak-depends-on-pkgs pak))))
  ;;           (format t "dependents: ~a~%" dependents)
  ;;           (let ((edges
  ;;                   (loop :for dep :in dependents
  ;;                         :for pak-indices-to-other-clusters
  ;;                           = (loop :for i :from 0 :to (1- (length all-elements))
  ;;                                   :for element = (aref all-elements i)
  ;;                                   :do (format t "element: ~a, i: ~a~%" element i)
  ;;                                   :when (and (typep element 'pak)
  ;;                                              (or (> i to-index)
  ;;                                                  (< i from-index))
  ;;                                              (equal dep (pak-name element)))
  ;;                                     :collect i)
  ;;                         :for indices-of-other-clusters
  ;;                           = (loop :for i :in pak-indices-to-other-clusters
  ;;                                   :for cluster-index
  ;;                                     = (loop :for j :from i :downto 0
  ;;                                             :when (member j cluster-indices :test #'=)
  ;;                                               :return j)
  ;;                                   :when cluster-index
  ;;                                     :collect cluster-index)
  ;;                         :do (format t "dep: ~a~%" dep)
  ;;                         :do (format t "index-refs-to-other-clusters: ~a~%" pak-indices-to-other-clusters)
  ;;                         :do (format t "indices-of-other-clusters: ~a~%" indices-of-other-clusters)
  ;;                         :append (loop :for index :in indices-of-other-clusters
  ;;                                       :collect (make-instance 'cl-dot:attributed
  ;;                                                               :object (aref all-elements index)
  ;;                                                               :attributes '(:weight 5
  ;;                                                                             :ltail "cluster_fooc"
  ;;                                                                             :lhead "cluster_barc"))))))
  ;;             (format t "edges: ~a~%" edges)
  ;;             (return-from graph-object-points-to edges)))))))
  nil)

(defun %find-indices-of-clusters (own-cluster elem-vec)
  (loop :with our-cluster-index = nil
        :with cluster-indices = '()
        :for i :from 0 :to (1- (length elem-vec))
        :for element = (aref elem-vec i)
        :if (typep element 'cluster)
          :do (push i cluster-indices)
        :if (eq element own-cluster)
          :do (setf our-cluster-index i)
        :finally
           (return (cons our-cluster-index
                         (reverse cluster-indices)))))

(defmethod graph-object-cluster ((graph (eql 'packages)) object)
  (format t "graph-object-cluster ~a~%" object)
  (typecase object
    (cluster nil)
    (node *current-cluster*)))
