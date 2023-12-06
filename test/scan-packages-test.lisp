(defpackage :graphy.scan-packages-test
            (:use :cl :fiveam :graphy.scan-packages)
            (:export #:run!
                     #:all-tests
                     #:nil))
(in-package :graphy.scan-packages-test)

(def-suite scan-packages-tests
           :description "Test for scanning packages"
           :in graphy.tests:test-suite)

(in-suite scan-packages-tests)

(test scan-packages--file-does-not-exist
  "Scan packages in source root, but the file does not exist"
  (signals simple-error (scan-project "test-projects/not-exists")))

(test scan-packages-none
  "Scan packages in source root, but there are no packages"
  (is (equalp '() (scan-project "test-projects/proj0"))))

(test scan-packages-one--but-no-file
  "Scan packages in source root, there is one package, but no file in it."
  (is (equalp `(,(make-pak :name "foo"))
              (scan-project "test-projects/proj1"))))

(test scan-packages-one--one-scala-file
  "Scan packages in source root, there is one package wit a scala file."
  (is (equalp `(,(make-pak :name "foo"))
              (scan-project "test-projects/proj2"))))

(test scan-packages-two-levels
  "Scan packages in source root, there are two levels of packages."
  (let ((scanned (scan-project "test-projects/proj3")))
    (inspectx scanned)
    (is (= 2 (length scanned)))
    (is-true (every (lambda (x)
                      (member x 
                              `(,(make-pak :name "foo")
                                ,(make-pak :name "foo.bar"))
                              :test #'equalp))
                    scanned))))

(test scan-packages-two-levels--two-on-second-level
  "Scan packages in source root, there are two levels of packages, and two packages on the second level."
  (let ((scanned (scan-project "test-projects/proj4")))
    (inspectx scanned)
    (is (= 3 (length scanned)))
    (is-true (every (lambda (x)
                      (member x 
                              `(,(make-pak :name "foo.bar")
                                ,(make-pak :name "foo")
                                ,(make-pak :name "foo.buzz"))
                              :test #'equalp))
                    scanned))))

(test scan-packages-two-levels--two-on-each-second-level
  "Scan packages in source root, there are three levels of packages."
  (let ((scanned (scan-project "test-projects/proj5")))
    (inspectx scanned)
    (is (= 6 (length scanned)))
    (is-true (every (lambda (x)
                      (member x 
                              `(,(make-pak :name "foo.bar")
                                ,(make-pak :name "foo")
                                ,(make-pak :name "foo.buzz")
                                ,(make-pak :name "foo2.bar")
                                ,(make-pak :name "foo2")
                                ,(make-pak :name "foo2.buzz"))
                              :test #'equalp))
                    scanned))))

(test scan-packages-three-levels
  "Scan packages in source root, there are three levels of packages."
  (let ((scanned (scan-project "test-projects/proj6")))
    (inspectx scanned)
    (is (= 7 (length scanned)))
    (is-true (every (lambda (x)
                      (member x 
                              `(,(make-pak :name "foo.bar")
                                ,(make-pak :name "foo")
                                ,(make-pak :name "foo.buzz")
                                ,(make-pak :name "foo.bar.buzz")
                                ,(make-pak :name "foo2.bar")
                                ,(make-pak :name "foo2")
                                ,(make-pak :name "foo2.buzz"))
                              :test #'equalp))
                    scanned))))

(test scan-packages--with-file-dependencies
  "Scans packages and also collects the package dependencies of each file/class.
There are two source files with identical package dependencies.
The result should be a list of packages with the dependencies of the two files merged."
  (let ((scanned (scan-project "test-projects/dep0" :collect-pak-deps t)))
    (inspectx scanned)
    (is (= 1 (length scanned)))
    (is-true (every
              (lambda (x)
                (member x 
                        `(,(make-pak :name "foo"
                                     :depends-on-pkgs '("foo.bar"
                                                        "foo2.bar"
                                                        "foo3.bar")))
                        :test #'equalp))
                    scanned))))

(test scan-packages-with-exclude-filter
  "Scan package with taking an exclude filter into account."
  (let ((scanned (scan-project "test-projects/proj6"
                               :exclude '("^foo2$" "foo\\."))))
    (inspectx scanned)
    (is (= 3 (length scanned)))
    (is-true (every (lambda (x)
                      (member x 
                              `(,(make-pak :name "foo")
                                ,(make-pak :name "foo2.bar")
                                ,(make-pak :name "foo2.buzz"))
                              :test #'equalp))
                    scanned))))

(test scan-packages-with-include-filter
  "Scan package with taking an include filter into account."
  (let ((scanned (scan-project "test-projects/proj6"
                               :include '("foo2.+"))))
    (inspectx scanned)
    (is (= 2 (length scanned)))
    (is-true (every (lambda (x)
                      (member x 
                              `(,(make-pak :name "foo2.bar")
                                ,(make-pak :name "foo2.buzz"))
                              :test #'equalp))
                    scanned))))

(test scan-packages-with-include-and-exclude-filter
  (let ((scanned (scan-project "test-projects/proj6"
                               :include '("foo2.+")
                               :exclude '("foo2.buzz"))))
    (inspectx scanned)
    (is (= 1 (length scanned)))
    (is-true (every (lambda (x)
                      (member x 
                              `(,(make-pak :name "foo2.bar"))
                              :test #'equalp))
                    scanned))))

(defun inspectx (x)
  "Just prints `X' and returns it."
  (print x)
  (terpri)
  x)
