;; vim: ft=lisp et

(in-package :asdf)

(defsystem :trestrul
  :description "Tiny utilities for TREe-STRUctured-List."
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "README.md"))
  :author "Shinichi Sato"
  :license "Public Domain"
  :components ((:file "trestrul")))

;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "trestrul"))))
 (test-system :trestrul.test))
