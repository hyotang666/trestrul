(in-package :cl-user)

(defpackage :trestrul
  (:use :cl)
  (:export ;;;; main api - tree functions
           #:mapleaf
           #:nmapleaf
           #:remove-leaf
           #:remove-leaf-if
           #:dotree
           #:collect-node
           #:asubst
           #:asubst-if
           #:ansubst
           #:ansubst-if
           #:find-leaf
           #:find-leaf-if
           #:find-node-if
           #:traverse
           #:path-to
           #:follow
           ;;;; types
           #:tree
           #:proper-tree
           ;;;; predicates
           #:treep
           #:proper-treep
           ;;;; conditions
           #:invalid-tree))

(in-package :trestrul)

(deftype tree ()
  "Tree structured list which includes NIL and dotted pair."
  'list)

(defun treep (arg) (listp arg))

(deftype proper-tree ()
  "Tree structured list but without dotted pair."
  '(satisfies proper-treep))

(defun proper-treep (arg)
  (labels ((rec (arg)
             (typecase arg
               (null t)
               (atom (return-from proper-treep nil))
               (t (body (car arg) (cdr arg)))))
           (body (first rest)
             (when (consp first)
               (rec first))
             (rec rest)))
    (rec arg)))

(define-condition invalid-tree (simple-type-error) ())

(deftype function-designator ()
  '(or function (and symbol (not (or boolean keyword)))))

(defun ensure-function (form)
  (typecase form
    ((cons (eql quote) (cons symbol null)) `#',(cadr form))
    ((or (cons (eql function) t) (cons (eql lambda) t)) form)
    (t `(coerce ,form 'function))))

;;;; MAPLEAF

(declaim
 (ftype (function (function-designator tree) (values tree &optional)) map-leaf))

(defun mapleaf (fun tree)
  (check-type tree tree)
  (%mapleaf (coerce fun 'function) tree))

(define-compiler-macro mapleaf
    (fun tree)
  `(%mapleaf ,(ensure-function fun) ,tree))

(declaim (ftype (function (function tree) (values tree &optional)) %mapleaf))

(defun %mapleaf (fun tree)
  (declare (type function fun)
           (type tree tree))
  (labels ((rec (tree)
             (cond ((null tree) tree)
                   ((atom tree) (values (funcall fun tree)))
                   (t (cons (rec (car tree)) (rec (cdr tree)))))))
    (rec tree)))

;;;; NMAPLEAF

(declaim
 (ftype (function (function-designator tree) (values tree &optional)) nmapleaf))

(defun nmapleaf (fun tree)
  (assert (typep tree 'tree))
  (%nmapleaf (coerce fun 'function) tree))

(define-compiler-macro nmapleaf
    (fun tree)
  `(%nmapleaf ,(ensure-function fun) ,tree))

(declaim (ftype (function (function tree) (values tree &optional)) %nmapleaf))

(defun %nmapleaf (fun tree)
  (declare (type function fun)
           (optimize (speed 3))
           (type tree tree))
  (labels ((rec (tree)
             (cond ((null tree) tree)
                   ((atom tree) (values (funcall fun tree)))
                   (t (rplaca tree (rec (car tree)))
                      (rplacd tree (rec (cdr tree)))))))
    (rec tree)))

(macrolet ((check (form type api)
             (let ((datum (gensym "DATUM")))
               `(let ((,datum ,form))
                  (assert (typep ,datum ',type) () 'invalid-tree
                    :format-control "~S: Must tree but ~S"
                    :format-arguments (list ',api ,datum)
                    :expected-type 'tree
                    :datum ,datum)))))
  (defun collect-node
         (target tree &key (key #'identity) (test #'eql) recursive-p)
    (check tree tree collect-node)
    (macrolet ((expand (rec-p)
                 `(labels ((rec (tree)
                             (etypecase tree
                               (null ; END OF PROPER LIST
                                (when (funcall test target (funcall key tree))
                                  (push tree acc)))
                               (atom ; END OF DOTTED LIST
                                nil) ; ignore leaf.
                               (cons ; PROGRESS
                                (body (car tree) (cdr tree)))))
                           (body (elt rest)
                             (if (listp elt)
                                 (elt-is-node elt rest)
                                 (rec rest))) ; ignore leaf.
                           (elt-is-node (node rest)
                             (if (funcall test target (funcall key node))
                                 (target-is-found node rest)
                                 (progn (rec node) (rec rest))))
                           (target-is-found (found rest)
                             ,(if (not rec-p)
                                  `(progn (push found acc) (rec rest))
                                  `(progn
                                    (push found acc)
                                    (when found
                                      (rec found))
                                    (rec rest)))))
                    (rec tree))))
      (let (acc)
        (if recursive-p
            (expand t)
            (expand nil))
        (nreverse acc)))))

(defun remove-leaf (item tree &key (test #'eql) (key #'identity) (keep t))
  (macrolet ((traverse (keep)
               (flet ((! (form)
                        `(handler-case ,form
                           (error ()
                             (error 'invalid-tree
                                    :format-control "REMOVE-LEAF accepts only proper tree, but ~S.~&About PROPER-TREE, evaluate (describe '~S)."
                                    :format-arguments (list tree 'proper-tree)
                                    :expected-type 'proper-tree
                                    :datum tree))))
                      (may-push (keep)
                        (if keep
                            `(push (rec (car first) (cdr first)) acc)
                            `(let ((it (rec (car first) (cdr first))))
                               (if it
                                   (push it acc)
                                   acc)))))
                 `(labels ((rec (first rest &optional acc)
                             (etypecase first
                               (cons
                                (rec (car rest) (cdr rest) ,(may-push keep)))
                               (null (nreverse acc))
                               (atom
                                (rec ,(! `(car rest)) (cdr rest)
                                     (if (funcall test item
                                                  (funcall key first))
                                         acc
                                         (push first acc)))))))
                    (rec ,(! `(car tree)) (cdr tree))))))
    (if keep
        (traverse t)
        (traverse nil))))

;;;; REMOVE-LEAF-IF

(declaim
 (ftype (function
         ((or symbol function) proper-tree &key (:key (or symbol function))
          (:keep boolean))
         (values proper-tree &optional))
        remove-leaf-if))

(defun remove-leaf-if (function tree &key (key #'identity) (keep t))
  (%remove-leaf-if (coerce function 'function) tree (coerce key 'function)
                   keep))

(define-compiler-macro remove-leaf-if
    (function tree &key (key '#'identity) (keep t))
  `(%remove-leaf-if ,(ensure-function function) ,tree ,(ensure-function key)
                    ,keep))

(declaim
 (ftype (function (function proper-tree function boolean)
         (values proper-tree &optional))
        %remove-leaf-if))

(defun %remove-leaf-if (function tree key keep)
  (declare (type function function key)
           (optimize speed))
  (macrolet ((traverse (keep)
               (flet ((! (form)
                        `(handler-case ,form
                           (error ()
                             (error 'invalid-tree
                                    :format-control "REMOVE-LEAF-IF accepts only proper tree, but ~S.~&About PROPER-TREE, evaluate (describe '~S)."
                                    :format-arguments (list tree 'proper-tree)
                                    :expected-type 'proper-tree
                                    :datum tree))))
                      (may-push (keep)
                        (if keep
                            `(push (rec (car first) (cdr first)) acc)
                            `(let ((it (rec (car first) (cdr first))))
                               (if it
                                   (push it acc)
                                   acc)))))
                 `(labels ((rec (first rest &optional acc)
                             (etypecase first
                               (cons
                                (rec (car rest) (cdr rest) ,(may-push keep)))
                               (null (nreverse acc))
                               (atom
                                (rec ,(! `(car rest)) (cdr rest)
                                     (if (funcall function (funcall key first))
                                         acc
                                         (push first acc)))))))
                    (rec ,(! `(car tree)) (cdr tree))))))
    (if keep
        (traverse t)
        (traverse nil))))

(defmacro dotree ((var tree &optional return) &body body)
  (labels ((split-declare (body)
             (loop :for sexp :in body
                   :when (typep sexp '(cons (eql declare) t))
                     :collect sexp :into declares
                   :else
                     :collect sexp :into bodies
                   :finally (return (values declares bodies))))
           (! (form node)
             `(handler-case ,form
                (error ()
                  (error 'invalid-tree
                         :format-control "~S: Form TREE must generate TREE, but ~S.~%About type TREE, evaluate (DESCRIBE '~S)."
                         :format-arguments (list 'dotree ,node 'tree)
                         :expected-type 'tree
                         :datum ,node)))))
    (let ((node (gensym "NODE"))
          (rest (gensym "REST"))
          (top (gensym "TOP"))
          (elt (gensym "ELT")))
      (multiple-value-bind (declares body)
          (split-declare body)
        `(prog* ((,node ,tree) (,elt ,(! `(car ,node) node))
                 (,rest (cdr ,node)))
          ,top
           (if (and (null ,rest) (null ,elt))
               (return
                (let (,var)
                  (declare (ignorable ,var))
                  ,return))
               (if (consp ,elt)
                   (setf ,rest (cons (cdr ,elt) ,rest)
                         ,elt (car ,elt))
                   (progn
                    (when ,elt
                      (let ((,var ,elt))
                        ,@declares
                        (tagbody ,@body)))
                    (if (and ,rest (atom ,rest))
                        (shiftf ,elt ,rest nil)
                        (setf ,elt (car ,rest)
                              ,rest (cdr ,rest))))))
           (go ,top))))))

(defun asubst (substituter target tree &key (test #'eql) (key #'identity))
  (if (funcall test target (funcall key tree))
      (funcall substituter tree)
      (if (atom tree)
          tree
          (cons (asubst substituter target (car tree) :test test :key key)
                (asubst substituter target (cdr tree) :test test :key key)))))

(defun asubst-if (substituter predicate tree &key (key #'identity))
  (if (funcall predicate (funcall key tree))
      (funcall substituter tree)
      (if (atom tree)
          tree
          (cons (asubst-if substituter predicate (car tree) :key key)
                (asubst-if substituter predicate (cdr tree) :key key)))))

(defun ansubst (substituter target tree &key (test #'eql) (key #'identity))
  (if (funcall test target (funcall key tree))
      (funcall substituter tree)
      (if (atom tree)
          tree
          (progn
           (rplaca tree
                   (ansubst substituter target (car tree) :test test :key key))
           (rplacd tree
                   (ansubst substituter target (cdr tree)
                            :test test
                            :key key))))))

(defun ansubst-if (substituter predicate tree &key (key #'identity))
  (if (funcall predicate (funcall key tree))
      (funcall substituter tree)
      (if (atom tree)
          tree
          (progn
           (rplaca tree (ansubst-if substituter predicate (car tree) :key key))
           (rplacd tree
                   (ansubst-if substituter predicate (cdr tree) :key key))))))

(defun find-leaf (target tree &key (test #'eql) (key #'identity))
  (dotree (var tree)
    (when (funcall test target (funcall key var))
      (return var))))

(defun traverse (function tree)
  (funcall function tree)
  (unless (atom tree)
    (catch 'traverse (traverse function (car tree)))
    (catch 'traverse (traverse function (cdr tree)))))

(defun find-leaf-if (pred tree &key (key #'identity))
  (dotree (var tree)
    (when (funcall pred (funcall key var))
      (return var))))

(defun find-node-if (pred tree &key (count 1) recursive-p)
  (check-type count (integer 1 *))
  (traverse
    (lambda (node)
      (when (and (listp node)
                 (funcall pred node)
                 (cond ((zerop (decf count)) t)
                       (recursive-p nil)
                       (t (throw 'traverse nil))))
        (return-from find-node-if node)))
    tree))

(defun path-to (item tree &key (test #'eql))
  (labels ((rec (tree &optional path)
             (when (funcall test item tree)
               (return-from path-to (values (nreverse path) t)))
             (if (atom tree)
                 (values nil nil)
                 (progn
                  (rec (car tree) (cons 'car path))
                  (rec (cdr tree) (cons 'cdr path))))))
    (rec tree)))

(defun follow (path tree)
  (if (null path)
      tree
      (reduce (lambda (tree path) (funcall path tree)) path
              :initial-value tree)))
