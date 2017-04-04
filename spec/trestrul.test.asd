; vim: ft=lisp et
(in-package :asdf)
(defsystem :trestrul.test
  :depends-on
  (:jingoh "trestrul")
  :components
  ((:file "trestrul"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine)))