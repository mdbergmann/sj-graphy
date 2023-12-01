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

(test scan-packages-none
  "Scan packages in source root, but there are no packages"
  (is (fset:equal? (fset:set) (scan-project "test-projects/proj0" :source))))

(test scan-packages-one--but-no-file
  "Scan packages in source root, there is one package, but no file in it."
  (is (fset:equal? (fset:set) (scan-project "test-projects/proj1" :source))))

(test scan-packages-one--one-scala-file
  "Scan packages in source root, there is one package wit a scala file."
  (is (fset:equal? (fset:set '(:package "foo")) (scan-project "test-projects/proj2" :source))))

(test scan-packages-two-levels
  "Scan packages in source root, there are two levels of packages."
  (is (fset:equal?
       (fset:set '(:package "foo.bar")
                 '(:package "foo"))
       (scan-project "test-projects/proj3" :source))))

(test scan-packages-two-levels--two-on-second-level
  "Scan packages in source root, there are two levels of packages, and two packages on the second level."
  (is (fset:equal?
       (fset:set '(:package "foo.bar")
                 '(:package "foo.buzz")
                 '(:package "foo"))
       (scan-project "test-projects/proj4" :source))))

(test scan-packages-two-levels--two-on-each-second-level
  "Scan packages in source root, there are three levels of packages."
  (is (fset:equal?
       (fset:set '(:package "foo.bar")
                 '(:package "foo.buzz")
                 '(:package "foo")
                 '(:package "foo2.bar")
                 '(:package "foo2.buzz")
                 '(:package "foo2"))
       (scan-project "test-projects/proj5" :source))))
