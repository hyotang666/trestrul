;; vim: ft=lisp et

(in-package :asdf)

(unless(uiop:featurep :doc-bootstrap)
  (pushnew :doc-bootstrap *features*)
  (defsystem :doc-bootstrap
     :defsystem-depends-on (:documentation-embedder)))

(defsystem :trestrul
  :version #.(demb:version :demb(first(demb:cached-file-lines "README.md")))
  :description #.(demb:description :demb(first(demb:cached-file-lines)))
  :long-description #.(format nil "~{~A~%~}"(cddr(demb:cached-file-lines)))
  :license #.(demb:license :demb(find-if (demb:searcher"* License")
                                         (demb:cached-file-lines)))
  :author "Shinichi Sato"
  :in-order-to((test-op(test-op :trestrul-test)))
  :depends-on(:documentation-embedder)
  :components ((:file "trestrul")))

(defsystem :trestrul-test
  :depends-on (:trestrul :jingoh)
  :components ((:file "design"))
  :perform(test-op(o c)
            (uiop:symbol-call :jingoh :report)))
