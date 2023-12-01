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
    (remove-duplicates
     (loop :for dir :in (uiop:subdirectories real-path)
           :for scanned-packages = (%scan-dir dir '())
           :if scanned-packages
             :append scanned-packages)
     :key #'second)))

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
               (reduce (lambda (accu pak)
                         (%conc-package-name accu (second pak)))
                       package-accu
                       :initial-value ""))
             (new-package (%conc-package-name prev-package package-name)))
        (setf package-accu
              (cons `(:package ,new-package) package-accu)))))
  (format t "package-accu ~a~%" package-accu)

  (let ((subdirs (uiop:subdirectories path)))
    (if subdirs
        (reduce (lambda (accu dir)
                  (append accu (%scan-dir dir package-accu)))
                subdirs
                :initial-value '())
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
