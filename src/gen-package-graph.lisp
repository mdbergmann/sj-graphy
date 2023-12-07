(defpackage :sj-graphy.gen-package-graph
  (:use :cl)
  (:nicknames :genpak)
  (:import-from #:spak
                #:pak
                #:make-pak
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

(defvar *current-cluster* nil "The current cluster cl-dot object.
When `MAKE-GRAPH' is called with a `CLUSTER' argument, this variable
is used to keep track of the current cluster object.")

(defvar *all-packages* nil "A list of all raw packages as given to `MAKE-GRAPH'.")
(defvar *replace-pkg-names* nil "A list of conses of the form (\"old-name\" . \"new-name\").")
(defvar *exclude-connections-to* nil "A list of package names that should not be connected, aka no edges be created.")

(defun make-graph (packages &key (cluster nil)
                              (replace-pkg-names nil)
                              (exclude-connections-to nil))
  "Generates a `cl-dot:graph' from a list of `spak:pak's.
If `CLUSTER' is non-nil, the graph will be clustered by the clusters.
So `PACKAGES' in this case is a plist like this:
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
        (*exclude-connections-to* exclude-connections-to))
    (if cluster
        (cl-dot:generate-graph-from-roots
         'packages
         (setf *all-packages*
               (reduce (lambda (accu cluster)
                         (append accu
                                 (list (make-cluster
                                        :name (getf cluster :name nil)
                                        :color (getf cluster :color nil)))
                                 (getf cluster :packages nil)))
                       packages
                       :initial-value nil)))
        (cl-dot:generate-graph-from-roots 'packages
                                          (setf *all-packages* packages)))))

(defmethod graph-object-node ((graph (eql 'packages)) (object cluster))
  (format t "node ~a~%" object)
  (setf *current-cluster*
        (make-instance 'cl-dot:cluster
                       :attributes `(:label ,(cluster-name object)
                                     :bgcolor ,(cluster-color object))))
  nil)

(defmethod graph-object-node ((graph (eql 'packages)) (object pak))
  (format t "node ~a~%" object)
  (make-instance 'cl-dot:node
                 :attributes `(:label ,(%maybe-replace-pkg-name (pak-name object))
                               :shape :box)))

(defun %maybe-replace-pkg-name (name)
  (if *replace-pkg-names*
      (let ((new-name name))
        (loop :for (old . new) :in *replace-pkg-names*
              :do (setf new-name (cl-ppcre:regex-replace-all old new-name new)))
        new-name)
      name))

(defmethod graph-object-points-to ((graph (eql 'packages)) (object pak))
  (format t "points-to ~a~%" object)
  (let* ((pak-deps (pak-depends-on-pkgs object))
         (dependents
           (loop :for pkg :in pak-deps
                 :for dependents = (loop :for pak :in *all-packages*
                                         :when (and (typep pak 'pak)
                                                    (equal pkg (pak-name pak)))
                                           :collect pak)
                 :append dependents)))
    (loop :for dep :in dependents
          :unless (member (pak-name dep) *exclude-connections-to* :test #'equal)
            :collect (make-instance 'cl-dot:attributed
                                    :object dep
                                    :attributes '(:weight 3)))))

(defmethod graph-object-points-to ((graph (eql 'packages)) (object cluster))
  (format t "points-to ~a~%" object)
  nil)

(defmethod graph-object-cluster ((graph (eql 'packages)) object)
  (format t "cluster ~a~%" object)
  (typecase object
    (cluster nil)
    (pak *current-cluster*)))
