;; vim: ft=lisp et

(in-package :asdf)

(defsystem :trestrul
  :description "Tiny utilities for TREe-STRUctured-List."
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname "README.md" *load-pathname*))
  :author "Shinichi Sato"
  :in-order-to((test-op(test-op :trestrul-test)))
  :components ((:file "trestrul")))

(defsystem :trestrul-test
  :depends-on (:trestrul :jingoh)
  :components ((:file "design"))
  :perform(test-op(o c)
            (uiop:symbol-call :jingoh :report)))
