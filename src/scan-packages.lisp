(defpackage :graphy.scan-packages
  (:use :cl)
  (:nicknames :spak)
  (:export #:scan-project
           #:scan-packages)
  )

(in-package :graphy.scan-packages)

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
                 (let ((paks (%scan-dir dir (fset:empty-set))))
                   (if (fset:nonempty? paks)
                       (fset:union accu paks)
                       accu)))
               (uiop:subdirectories path)
               :initial-value (fset:empty-set)))

(defun %ensure-proper-path (path)
  (if (str:ends-with-p "/" path)
      path
      (concatenate 'string path "/")))

(defun %scan-dir (path package-accu)
  "Recursively scans path for subdirectories and returns a list of packages."
  (let ((files (%filter-for-file-extension
                (uiop:directory-files path)
                ".scala")))
    ;;(format t "scanned files in ~a: ~%~a" path files)
    (when (car files)
      (let* ((package-name
               (first (last (pathname-directory path))))
             (prev-package
               (fset:reduce (lambda (accu pak)
                              (%conc-package-name accu (second pak)))
                            package-accu
                            :initial-value ""))
             (new-package (%conc-package-name prev-package package-name)))
        (setf package-accu
              (fset:with package-accu `(:package ,new-package))))))
  (format t "package-accu ~a~%" package-accu)

  (let ((subdirs (uiop:subdirectories path)))
    (if subdirs
        (fset:reduce (lambda (accu dir)
                       (let ((paks (%scan-dir dir package-accu)))
                         (if (fset:nonempty? paks)
                             (fset:union accu paks)
                             accu)))
                     subdirs
                     :initial-value (fset:empty-set))
        package-accu)))

(defun %filter-for-file-extension (files extension)
  (remove-if-not (lambda (file)
                   (let ((filename (file-namestring file)))
                     (str:ends-with-p extension filename)))
                 files))

(defun %conc-package-name (prev new)
  (concatenate 'string
               prev
               (if (string= "" prev) "" ".")
               new))
