(asdf:defsystem cffi-j
  :version "0"
  :description "CFFI binding for J language engine"
  :maintainer " <ramarren@cignet.higersbergernet>"
  :author " <ramarren@cignet.higersbergernet>"
  :licence "BSD-style"
  :depends-on (:iterate :cffi :alexandria :babel)
  :components ((:file "package")
	       (:file "bindings" :depends-on ("package" "jdll"))
	       (:file "jdll" :depends-on ("package"))
	       (:file "wrapper" :depends-on ("package" "jdll" "bindings"))))

