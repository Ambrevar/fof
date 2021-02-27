(uiop:define-package fof/package
  (:nicknames fof)
  (:documentation "File object finder.
This is the meta package which includes all others.")
  (:use-reexport
   #:fof/file
   #:fof/predicates))
