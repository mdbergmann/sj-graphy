(defpackage :graphy.gen-package-graph
  (:use :cl)
  (:nicknames :genpak)
  (:import-from #:spak
                #:pak
                #:make-pak
                #:pak-name
                #:pak-module)
  (:shadowing-import-from #:cl-dot
                          #:graph-object-node
                          #:graph-object-cluster)
  (:export #:make-graph)
  )

(in-package :graphy.gen-package-graph)

(defstruct cluster name color)

(defvar *current-cluster* nil "The current cluster cl-dot object.
When `MAKE-GRAPH' is called with a `CLUSTER' argument, this variable
is used to keep track of the current cluster object.")

(defun make-graph (packages &key (cluster nil))
  "Generates a `cl-dot:graph' from a list of `spak:pak's."
  (setf *current-cluster* nil)
  (if cluster
      (cl-dot:generate-graph-from-roots
       'packages
       (let ((result nil))
         (loop :for cluster :in packages
               :for name = (getf cluster :name nil)
               :for color = (getf cluster :color nil)
               :for packages = (getf cluster :packages nil)
               :do (progn
                     (setf result
                           (append result
                                   (list (make-cluster :name name :color color))
                                   packages))))
         result))
      (cl-dot:generate-graph-from-roots 'packages packages)))

(defmethod graph-object-node ((graph (eql 'packages)) (object cluster))
  (format t "node cluster ~a~%" object)
  (setf *current-cluster*
        (make-instance 'cl-dot:cluster
                       :attributes `(:label ,(cluster-name object)
                                     :color ,(cluster-color object))))
  nil)

(defmethod graph-object-node ((graph (eql 'packages)) (object pak))
  (format t "node pak ~a~%" object)
  (make-instance 'cl-dot:node
                 :attributes `(:label ,(pak-name object)
                               :shape :box)))

(defmethod graph-object-cluster ((graph (eql 'packages)) object)
  (format t "cluster ~a~%" object)
  (typecase object
    (cluster nil)
    (pak *current-cluster*)))
