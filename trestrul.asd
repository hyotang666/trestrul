;; vim: ft=lisp et

(in-package :asdf)

(defsystem :trestrul
  :version "0.0.2"
  :description "Tiny utilities for TREe-STRUctured-List."
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "README.md"))
  :author "Shinichi Sato"
  :license "Public Domain"
  :components ((:file "trestrul")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "trestrul"))))
  (append (call-next-method)'((test-op "trestrul.test"))))
(defmethod operate :around ((o test-op)(c (eql (find-system "trestrul")))
                            &key ((:compile-print *compile-print*))
                            ((:compile-verbose *compile-verbose*))
                            &allow-other-keys)
  (call-next-method))
