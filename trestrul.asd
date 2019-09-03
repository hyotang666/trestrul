;; vim: ft=lisp et

(in-package :asdf)

(defsystem :trestrul
  :version "0.0.4"
  :description "Tiny utilities for TREe-STRUctured-List."
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "README.md"))
  :author "Shinichi Sato"
  :license "Public Domain"
  :components ((:file "trestrul")))

;; These forms below are added by JINGOH.GENERATOR.
(in-package :asdf)
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "trestrul"))))
  (append (call-next-method) '((test-op "trestrul.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "trestrul")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
(let ((system (find-system "jingoh.documentizer" nil)))
  (when(and system
            (not(featurep :clisp)))
    (load-system system)
    (defmethod operate :around
               ((o load-op) (c (eql (find-system "trestrul"))) &key)
      (let* ((forms nil)
             (*macroexpand-hook*
              (let ((outer-hook *macroexpand-hook*))
                (lambda (expander form env)
                  (when (typep form '(cons (eql defpackage) *))
                    (push form forms))
                  (funcall outer-hook expander form env))))
             (*default-pathname-defaults*
              (merge-pathnames "spec/" (system-source-directory c))))
        (multiple-value-prog1 (call-next-method)
          (mapc (find-symbol (string :importer) :jingoh.documentizer)
                forms))))))
