(defpackage :graphy.scan-packages
  (:use :cl)
  (:nicknames :spak)
  (:export #:scan-project
           #:scan-packages
           #:*file-spec*
           #:*default-source-root*
           #:*source-file-type*
           ;; the type for package
           #:pak
           #:make-pak
           #:pak-name
           #:pak-module)
  )

(in-package :graphy.scan-packages)

(defvar *default-source-root* "src")
(defvar *source-file-type* "scala"
  "Part of the source root. E.g. 'scala' or 'java'.")
(defparameter *search-file-spec* '(".scala")
  "A list of file extensions to search for. E.g. '*.scala' or '*.java'.")

(defvar *collect-package-deps* nil
  "If true, the dependencies of the packages are collected as well.")

(defstruct pak
  (name nil :type (or null string))
  (pak-imports nil :type list))

(defun scan-project (path &key
                            (source-type :source)
                            (collect-pak-deps nil))
  "`PATH' is the root path to a Java/Scala project whichg has a folder structure of
'src/main/scala' or 'src/test/scala' beneath.
Where `SOURCE-TYPE' defines the 'main or 'test' part.
Specify `:source' for 'main' and `:test' for 'test'.
`COLLECT-PAK-DEPS' is a boolean value that indicates whether the dependencies of the packages
should be collected as well. In the `PAK' structure, the field `PAK-IMPORTS' contains those as part of the result.
Those are just package dependencies, not class dependencies."
  (check-type path string)
  
  (setf path (%ensure-no-trailing-slash path))

  ;;(format t "~%path: ~a~%" path)
  (let* ((source-type-dir (ecase source-type
                            (:source "main")
                            (:test "test")))
         (real-path (make-pathname
                     :directory
                     (list :relative
                           path
                           *default-source-root*
                           source-type-dir
                           *source-file-type*))))
    (format t "real-path: ~a~%" real-path)
    (assert (probe-file real-path) nil "Path ~a does not exist." real-path)
    (let ((*collect-package-deps* collect-pak-deps))
      (scan-packages real-path))))

(defun %ensure-no-trailing-slash (path)
  (if (str:ends-with-p "/" path)
      (subseq path 0 (1- (length path)))
      path))

(defun scan-packages (path)
  "Scans for packages (which are folders that contains a source file) in the given path."
  (check-type path (or pathname string))
  (when (stringp path)
    (setf path (pathname path)))
  (assert (probe-file path) nil "Path ~a does not exist." path)

  ;;(format t "real-path: ~a~%" real-path)
  (fset:convert 'list
                (fset:reduce
                 (lambda (accu dir)
                   (let ((paks (%scan-packages dir "" (fset:empty-set))))
                     (if (fset:nonempty? paks)
                         (fset:union accu paks)
                         accu)))
                 (uiop:subdirectories path)
                 :initial-value (fset:empty-set))))

(defun %scan-packages (path current-package package-accu)
  "Recursively scans path for subdirectories and returns a list of packages.
`PATH' is the path to scan.
`CURRENT-PACKAGE' is the current package name, or position in the folder tree.
`PACKAGE-ACCU' is the accumulator for the packages found so far.
Returns a list of packages."
  (let ((files (%filter-for-file-spec
                (uiop:directory-files path)
                *search-file-spec*))
        (new-current-package)
        (package-name
          (first (last (pathname-directory path))))
        (file-pak-deps nil))

    (when *collect-package-deps*
      (setf file-pak-deps (%collect-package-deps files)))
      
      ;; extract package name
    (setf new-current-package
          (%conc-package-name current-package package-name)
          package-accu
          (fset:with package-accu (make-pak :name new-current-package
                                            :pak-imports file-pak-deps)))

    ;;(format t "package-accu ~a~%" package-accu)

    (flet ((descent-to-subfolders ()
             (let ((subdirs (uiop:subdirectories path)))
               (if subdirs
                   (fset:reduce (lambda (accu dir)
                                  (let ((paks (%scan-packages
                                               dir new-current-package package-accu)))
                                    (if (fset:nonempty? paks)
                                        (fset:union accu paks)
                                        accu)))
                                subdirs
                                :initial-value (fset:empty-set))
                   package-accu))))
      (descent-to-subfolders))))

(defun %collect-package-deps (files)
  (fset:convert
   'list
   (fset:reduce
    (lambda (accu file)
      (let ((deps (%collect-file-package-deps file)))
        (if (fset:nonempty? deps)
            (fset:union accu deps)
            accu)))
    files
    :initial-value (fset:empty-set))))

(defun %collect-file-package-deps (file)
  (let ((deps (fset:empty-set)))
    (with-open-file (in file)
      (loop :for line = (str:trim (read-line in nil nil))
            :while line
            :if (str:starts-with-p "import" line)
              :do
                 (multiple-value-bind (match res)
                     (ppcre:scan-to-strings
                      "(?-i)^import\\s+([a-z0-9\\.]*)($|\\.[A-Z\\{].*$)"
                      line)
                   (when (and match (not (null res)))
                     (setf deps (fset:with deps (elt res 0)))))))
    deps))

(defun %filter-for-file-spec (files file-spec)
  (remove-if-not (lambda (file)
                   (let ((filename (file-namestring file)))
                     (some (lambda (extension)
                             (str:ends-with-p extension filename))
                           file-spec)))
                 files))

(defun %conc-package-name (prev new)
  (concatenate 'string
               prev
               (if (string= "" prev) "" ".")
               new))
