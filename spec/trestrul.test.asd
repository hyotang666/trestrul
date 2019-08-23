; vim: ft=lisp et
(in-package :asdf)
(defsystem :trestrul.test
  :version "0.0.2"
  :depends-on
  (:jingoh "trestrul")
  :components
  ((:file "trestrul"))
  :perform
  (test-op (o c)
           (declare(special args))
           (apply #'symbol-call :jingoh :examine :trestrul args)))
