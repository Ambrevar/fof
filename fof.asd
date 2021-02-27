#-asdf3.1 (error "`fof' requires ASDF 3.1")

(defsystem "fof"
  :version "0.0.0"
  :author "Pierre Neidhardt <mail@ambrevar.xyz>"
  :licence "GPL3+"
  :class :package-inferred-system
  :depends-on ("fof/package"))
