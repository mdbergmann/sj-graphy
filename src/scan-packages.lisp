(defpackage :graphy.scan-packages
  (:use :cl)
  (:nicknames :spak)
  (:export #:scan-packages)
  )

(in-package :graphy.scan-packages)

(defun scan-packages (path source-type)
  (check-type path string)

  (unless (str:ends-with-p "/" path)
    (setf path (concatenate 'string path "/")))

  ;;(format t "~%path: ~a~%" path)
  (let ((real-path (case source-type
                     (:source
                      (uiop:subpathname path "src/main/scala"))
                     ;; maybe :test
                     )))
    ;;(format t "real-path: ~a~%" real-path)
    (fset:reduce (lambda (accu dir)
                   (let ((paks (%scan-dir dir (fset:empty-set))))
                     (if (fset:nonempty? paks)
                         (fset:union accu paks)
                         accu)))
                 (uiop:subdirectories real-path)
                 :initial-value (fset:empty-set))))

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
