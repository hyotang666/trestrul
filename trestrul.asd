;; vim: ft=lisp et

(in-package :asdf)

(defsystem :trestrul
  :description "Tiny utilities for TREe-STRUctured-List."
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "README.md"))
  :author "Shinichi Sato"
  :license "Public Domain"
  :components ((:file "trestrul")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "trestrul"))))
  (append (call-next-method)'((test-op "trestrul.test"))))
