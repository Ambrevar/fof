(uiop:define-package fof/package
  (:nicknames fof)
  (:documentation "File object finder.
This is the meta package which includes all others.")
  (:import-from #:trivial-package-local-nicknames)
  (:use-reexport
   #:fof/file
   #:fof/predicates))
