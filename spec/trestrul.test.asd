; vim: ft=lisp et
(in-package :asdf)
(defsystem :trestrul.test
  :version "0.0.0"
  :depends-on
  (:jingoh "trestrul")
  :components
  ((:file "trestrul"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :trestrul)))
