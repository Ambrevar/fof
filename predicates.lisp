(uiop:define-package fof/p              ; TODO: Name?
  (:documentation "All predicates for the FOF finder.")
  (:use #:common-lisp)
  ;; (:import-from #:alexandria)
  ;; (:import-from #:hu.dwim.defclass-star #:defclass*)
  (:import-from #:local-time)
  ;; (:import-from #:magicffi)
  ;; (:import-from #:serapeum #:export-always)
  (:import-from #:str)
  (:import-from #:trivia #:match))
(in-package fof/p)
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
;;   (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))


(defun match-date< (timestamp)
  "Return a file predicate that matches on modification time #'< than timestamp."
  (lambda (file)
    (local-time:timestamp< (modification-date file) timestamp)))

(defun match-date> (timestamp)
  "Return a file predicate that matches on modification time #'> than timestamp."
  (lambda (file)
    (local-time:timestamp> (modification-date file) timestamp)))

(export-always 'match-extension)
(defun match-extension (extension &rest more-extensions)
  "Return a predicate for files that match on of the provided extensions.
Useful for `finder'."
  (lambda (file)
    (some (lambda (ext)
            (string= ext (extension file)))
          (cons extension more-extensions))))

(export-always 'match-path)
(defun match-path (path-element &rest more-path-elements)
  "Return a predicate that matches when one of the path elements is contained in
the file path.
Useful for `finder'."
  (lambda (file)
    (some (lambda (elem)
            (str:contains? elem (path file)))
          (cons path-element more-path-elements))))

(export-always 'match-path-end)
(defun match-path-end (path-suffix &rest more-path-suffixes)
  "Return a predicate that matches when one of the path suffixes is contained in
the file path.
Useful for `finder'."
  (lambda (file)
    (some (lambda (suffix)
            (str:ends-with? (namestring suffix) (path file)))
          (cons path-suffix more-path-suffixes))))

(export-always 'match-name)
(defun match-name (name &rest more-names)
  "Return a predicate that matches when one of the names is contained in the
file basename.
Basename includes the extension.  Useful for `finder'."
  (lambda (file)
    (some (lambda (name)
            (str:contains? name (basename file)))
          (cons name more-names))))

;; TODO: Better control to filter in/out directories?
;; (export-always 'match-directory)
;; (defun match-directory (&key (empty? t) (non-empty? t) (files? t))
;;   "Return a predicate that matches on directories.
;; If target is a file, return FILES?.
;; Useful for `walk'."
;;   (lambda (directory)
;;     (if (uiop:directory-exists-p directory)
;;         (let ((files-or-dirs? (or (uiop:directory-files directory)
;;                                   (uiop:subdirectories directory))))
;;           (or (and empty?
;;                    (not files-or-dirs?))
;;               (and non-empty?
;;                    files-or-dirs?)))
;;         files?)))

(export-always 'match-executable)
(defun match-executable ()
  (lambda (file)
    (intersection
     (permissions file)
     '(:user-exec :group-exec :other-exec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export-always 'match-elf-binary)
(defun match-elf-binary ()
  (lambda (file)
    (and (slot-boundp file 'mime-type)
         (string= "application/x-executable" (mime-type file)))))

(export-always 'match-elf-library)
(defun match-elf-library ()
  (lambda (file)
    (and (slot-boundp file 'mime-type)
         (ppcre:scan "application/x-sharedlib" (first (mime-type file))))))
