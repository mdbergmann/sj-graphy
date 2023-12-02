(defpackage :graphy.scan-packages
  (:use :cl)
  (:nicknames :spak)
  (:export #:scan-project
           #:scan-packages
           #:*file-spec*)
  )

(in-package :graphy.scan-packages)

(defvar *file-spec* '("*.scala"))

(defun scan-project (path source-type)
  "`PATH' is the root path to a Java/Scala project whichg has a folder structure of
'src/main/scala' or 'src/test/scala' beneath.
Where `SOURCE-TYPE' defines the 'main or 'test' part.
Specify `:source' for 'main' and `:test' for 'test'."
  (check-type path string)
  
  (setf path (%ensure-proper-path path))

  ;;(format t "~%path: ~a~%" path)
  (let ((real-path (case source-type
                     (:source
                      (uiop:subpathname path "src/main/scala"))
                     ;; maybe :test
                     )))
    (scan-packages real-path)))

    
(defun scan-packages (path)
  "Scans for packages (which are folders that contains a source file) in the given path."
  (check-type path pathname)

  ;;(format t "real-path: ~a~%" real-path)
  (fset:reduce (lambda (accu dir)
                 (let ((paks (%collect-packages dir "" (fset:empty-set))))
                   (if (fset:nonempty? paks)
                       (fset:union accu paks)
                       accu)))
               (uiop:subdirectories path)
               :initial-value (fset:empty-set)))

(defun %ensure-proper-path (path)
  (if (str:ends-with-p "/" path)
      path
      (concatenate 'string path "/")))

(defun %collect-packages (path current-package package-accu)
  "Recursively scans path for subdirectories and returns a list of packages.
`PATH' is the path to scan.
`CURRENT-PACKAGE' is the current package name, or position in the folder tree.
`PACKAGE-ACCU' is the accumulator for the packages found so far.
Returns a list of packages."
  (let ((files (%filter-for-file-spec
                (uiop:directory-files path)
                *file-spec*))
        (new-current-package)
        (package-name
          (first (last (pathname-directory path)))))
    (declare (ignore files))      ; ignore for now. maybe needed later

    ;; extract package name
    (setf new-current-package
          (%conc-package-name current-package package-name)
          package-accu
          (fset:with package-accu `(:package ,new-current-package)))

    (format t "package-accu ~a~%" package-accu)

    (flet ((descent-to-subfolders ()
             (let ((subdirs (uiop:subdirectories path)))
               (if subdirs
                   (fset:reduce (lambda (accu dir)
                                  (let ((paks (%collect-packages dir new-current-package package-accu)))
                                    (if (fset:nonempty? paks)
                                        (fset:union accu paks)
                                        accu)))
                                subdirs
                                :initial-value (fset:empty-set))
                   package-accu))))
      (descent-to-subfolders))))

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
