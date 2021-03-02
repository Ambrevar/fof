(uiop:define-package fof/predicates              ; TODO: Name?
  (:nicknames fof/p)
  (:documentation "All predicates for the FOF finder.")
  (:use #:common-lisp
        #:fof/file)
  ;; (:import-from #:alexandria)
  ;; (:import-from #:hu.dwim.defclass-star #:defclass*)
  (:import-from #:local-time)
  ;; (:import-from #:magicffi)
  (:import-from #:serapeum #:export-always)
  (:import-from #:str)
  (:import-from #:trivia #:match))
(in-package fof/p)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

(export-always 'date<)
(defun date< (timestamp)
  "Return a file predicate that matches on modification time less recent than TIMESTAMP."
  (lambda (file)
    (local-time:timestamp< (modification-date file) timestamp)))

(export-always 'date>)
(defun date> (timestamp)
  "Return a file predicate that matches on modification time more recent than TIMESTAMP."
  (lambda (file)
    (local-time:timestamp> (modification-date file) timestamp)))

(export-always 'extension=)
(defun extension= (extension &rest more-extensions)
  "Return a predicate for files that match one of the provided extensions."
  (lambda (file)
    (some (sera:equals (extension file))
          (cons extension more-extensions))))

(export-always 'path~)
(defun path~ (path-element &rest more-path-elements)
  "Return a predicate that matches when one of the path elements is contained in
the file path."
  (apply #'fof/file::match-path path-element more-path-elements))

(export-always 'path$)
(defun path$ (path-suffix &rest more-path-suffixes)
  "Return a predicate that matches when one of the path suffixes matches
the file path."
  (apply #'fof/file::match-path-end path-suffix more-path-suffixes))

(export-always 'name~)
(defun name~ (name &rest more-names)
  "Return a predicate that matches when one of the names is contained in the
file basename. "
  (lambda (file)
    (some (lambda (name)
            (str:contains? name (basename file)))
          (cons name more-names))))

(defun depth< (level &optional (root (file *default-pathname-defaults*)))
  "Return a predicate that matches when the argument file is in a subdirectory
of ROOT less deep than LEVEL."
  (apply #'fof/file::match-depth< level root))

(export-always 'executable?)
(defun executable? (file)
  (intersection
   (permissions file)
   '(:user-exec :group-exec :other-exec)))

(export-always 'hidden?)
(defun hidden? (file)
  (str:starts-with? "." (basename file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export-always 'elf-binary?)
(defun elf-binary? (file)
  (and (slot-boundp file 'mime-type)
       (string= "application/x-executable" (mime-type file))))

(export-always 'elf-library?)
(defun elf-library? (file)
  (and (slot-boundp file 'mime-type)
       (ppcre:scan "application/x-sharedlib" (first (mime-type file)))))
