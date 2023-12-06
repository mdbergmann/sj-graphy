(defpackage :graphy.gen-package-graph
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

(in-package :graphy.gen-package-graph)

(defstruct cluster name color)

(defvar *current-cluster* nil "The current cluster cl-dot object.
When `MAKE-GRAPH' is called with a `CLUSTER' argument, this variable
is used to keep track of the current cluster object.")

(defvar *all-packages* nil "A list of all raw packages as given to `MAKE-GRAPH'.")

(defun make-graph (packages &key (cluster nil))
  "Generates a `cl-dot:graph' from a list of `spak:pak's."
  (setf *current-cluster* nil)

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
                                        (setf *all-packages* packages))))

(defmethod graph-object-node ((graph (eql 'packages)) (object cluster))
  (format t "node cluster ~a~%" object)
  (setf *current-cluster*
        (make-instance 'cl-dot:cluster
                       :attributes `(:label ,(cluster-name object)
                                     :bgcolor ,(cluster-color object))))
  nil)

(defmethod graph-object-node ((graph (eql 'packages)) (object pak))
  (format t "node pak ~a~%" object)
  (make-instance 'cl-dot:node
                 :attributes `(:label ,(pak-name object)
                               :shape :box)))

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
    (mapcar (lambda (dep)
              (make-instance 'cl-dot:attributed
                             :object dep
                             :attributes '(:weight 3)))
            dependents)))

(defmethod graph-object-cluster ((graph (eql 'packages)) object)
  (format t "cluster ~a~%" object)
  (typecase object
    (cluster nil)
    (pak *current-cluster*)))
