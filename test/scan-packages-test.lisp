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
  (is-true
   (every (lambda (x)
            (member x 
                    `(,(make-pak :name "foo")
                      ,(make-pak :name "foo.bar"))
                    :test #'equalp))
          (scan-project "test-projects/proj3"))))

(test scan-packages-two-levels--two-on-second-level
  "Scan packages in source root, there are two levels of packages, and two packages on the second level."
  (is-true
   (every (lambda (x)
            (member x 
                    `(,(make-pak :name "foo.bar")
                      ,(make-pak :name "foo")
                      ,(make-pak :name "foo.buzz"))
                    :test #'equalp))
          (scan-project "test-projects/proj4"))))

(test scan-packages-two-levels--two-on-each-second-level
  "Scan packages in source root, there are three levels of packages."
  (is-true
   (every (lambda (x)
            (member x 
                    `(,(make-pak :name "foo.buzz")
                      ,(make-pak :name "foo")
                      ,(make-pak :name "foo.bar")
                      ,(make-pak :name "foo2.bar")
                      ,(make-pak :name "foo2")
                      ,(make-pak :name "foo2.buzz"))
                    :test #'equalp))
          (scan-project "test-projects/proj5"))))

(test scan-packages-three-levels
  "Scan packages in source root, there are three levels of packages."
  (is-true
   (every (lambda (x)
            (member x 
                    `(,(make-pak :name "foo.buzz")
                      ,(make-pak :name "foo")
                      ,(make-pak :name "foo.bar.buzz")
                      ,(make-pak :name "foo.bar")
                      ,(make-pak :name "foo2.bar")
                      ,(make-pak :name "foo2")
                      ,(make-pak :name "foo2.buzz"))
                    :test #'equalp))
          (scan-project "test-projects/proj6"))))

(test scan-packages--with-file-dependencies
  "Scans packages and also collects the package dependencies of each file/class.
There are two source files with identical package dependencies.
The result should be a list of packages with the dependencies of the two files merged."
  (is (equalp
       `(,(make-pak :name "foo"
                    :depends-on-pkgs '("foo.bar"
                                       "foo2.bar"
                                       "foo3.bar")))
       (scan-project "test-projects/dep0" :collect-pak-deps t))))

(test scan-packages-with-exclude-filter
  "Scan package with taking an exclude filter into account."
  (is-true
   (every (lambda (x)
            (member x 
                    `(,(make-pak :name "foo2.bar")
                      ,(make-pak :name "foo2.buzz"))
                    :test #'equalp))
          (inspectx
           (scan-project "test-projects/proj6"
                         :exclude '("^foo2$" "foo"))))))

(defun inspectx (x)
  (print x)
  (terpri)
  x)
