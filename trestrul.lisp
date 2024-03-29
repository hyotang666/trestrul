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

(declaim (optimize speed))

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

;;;; MAPLEAF

(declaim
 (ftype (function (function-designator tree) (values tree &optional)) map-leaf))

(defun mapleaf (fun tree &aux (fun (coerce fun 'function)))
  (check-type tree tree)
  (labels ((rec (tree)
             (cond ((null tree) tree)
                   ((atom tree) (values (funcall fun tree)))
                   (t (cons (rec (car tree)) (rec (cdr tree)))))))
    (rec tree)))

;;;; NMAPLEAF

(declaim
 (ftype (function (function-designator tree) (values tree &optional)) nmapleaf))

(defun nmapleaf (fun tree &aux (fun (coerce fun 'function)))
  (assert (typep tree 'tree))
  (labels ((rec (tree)
             (cond ((null tree) tree)
                   ((atom tree) (values (funcall fun tree)))
                   (t
                    (rplaca tree (rec (car tree)))
                    (rplacd tree (rec (cdr tree)))))))
    (rec tree)))

(macrolet ((check (form type api)
             (let ((datum (gensym "DATUM")))
               `(let ((,datum ,form))
                  (assert (typep ,datum ',type) ()
                    'invalid-tree :format-control "~S: Must tree but ~S"
                                  :format-arguments (list ',api ,datum)
                                  :expected-type 'tree
                                  :datum ,datum)))))
  (defun collect-node
         (target tree
          &key (key #'identity) (test #'eql) recursive-p
          &aux (key (coerce key 'function)) (test (coerce test 'function)))
    (check tree tree collect-node)
    (macrolet ((expand (rec-p)
                 `(labels ((rec (tree)
                             (etypecase tree
                               (null ; END OF PROPER LIST
                                (when (funcall test target (funcall key tree))
                                  (rplacd tail (setf tail (list tree)))))
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
                                  `(progn
                                    (rplacd tail (setf tail (list found)))
                                    (rec rest))
                                  `(progn
                                    (rplacd tail (setf tail (list found)))
                                    (when found
                                      (rec found))
                                    (rec rest)))))
                    (rec tree))))
      (let* ((acc (cons :head nil)) (tail acc))
        (if recursive-p
            (expand t)
            (expand nil))
        (cdr acc)))))

(defun remove-leaf
       (item tree
        &key (test #'eql) (key #'identity) (keep t)
        &aux (test (coerce test 'function)) (key (coerce key 'function)))
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
                            `(rplacd tail
                                     (setf tail
                                             (list
                                               (rec (car first) (cdr first)))))
                            `(let ((it (rec (car first) (cdr first))))
                               (when it
                                 (rplacd tail (setf tail (list it))))))))
                 `(labels ((rec
                               (first rest
                                &optional (acc (cons nil nil)) (tail acc))
                             (etypecase first
                               (cons
                                ,(may-push keep)
                                (rec (car rest) (cdr rest) acc tail))
                               (null (cdr acc))
                               (atom
                                (if (funcall test item (funcall key first))
                                    acc
                                    (rplacd tail (setf tail (list first))))
                                (rec ,(! `(car rest)) (cdr rest) acc tail)))))
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

(defun remove-leaf-if
       (function tree
        &key (key #'identity) (keep t)
        &aux (function (coerce function 'function))
        (key (coerce key 'function)))
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
                            `(rplacd tail
                                     (setf tail
                                             (list
                                               (rec (car first) (cdr first)))))
                            `(let ((it (rec (car first) (cdr first))))
                               (when it
                                 (rplacd tail (setf tail (list it))))))))
                 `(labels ((rec
                               (first rest
                                &optional (acc (cons nil nil)) (tail acc))
                             (etypecase first
                               (cons
                                ,(may-push keep)
                                (rec (car rest) (cdr rest) acc tail))
                               (null (cdr acc))
                               (atom
                                (if (funcall function (funcall key first))
                                    acc
                                    (rplacd tail (setf tail (list first))))
                                (rec ,(! `(car rest)) (cdr rest) acc tail)))))
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

(defun asubst
       (substituter target tree
        &key (test #'eql) (key #'identity)
        &aux (test (coerce test 'function)) (key (coerce key 'function))
        (substituter (coerce substituter 'function)))
  (labels ((rec (tree)
             (if (funcall test target (funcall key tree))
                 (funcall substituter tree)
                 (if (atom tree)
                     tree
                     (cons (rec (car tree)) (rec (cdr tree)))))))
    (rec tree)))

(defun asubst-if
       (substituter predicate tree
        &key (key #'identity)
        &aux (key (coerce key 'function))
        (substituter (coerce substituter 'function))
        (predicate (coerce predicate 'function)))
  (labels ((rec (tree)
             (if (funcall predicate (funcall key tree))
                 (funcall substituter tree)
                 (if (atom tree)
                     tree
                     (cons (rec (car tree)) (rec (cdr tree)))))))
    (rec tree)))

(defun ansubst
       (substituter target tree
        &key (test #'eql) (key #'identity)
        &aux (substituter (coerce substituter 'function))
        (test (coerce test 'function)) (key (coerce key 'function)))
  (labels ((rec (tree)
             (if (funcall test target (funcall key tree))
                 (funcall substituter tree)
                 (if (atom tree)
                     tree
                     (progn
                      (rplaca tree (rec (car tree)))
                      (rplacd tree (rec (cdr tree))))))))
    (rec tree)))

(defun ansubst-if
       (substituter predicate tree
        &key (key #'identity)
        &aux (substituter (coerce substituter 'function))
        (predicate (coerce predicate 'function)) (key (coerce key 'function)))
  (labels ((rec (tree)
             (if (funcall predicate (funcall key tree))
                 (funcall substituter tree)
                 (if (atom tree)
                     tree
                     (progn
                      (rplaca tree (rec (car tree)))
                      (rplacd tree (rec (cdr tree))))))))
    (rec tree)))

(defun find-leaf
       (target tree
        &key (test #'eql) (key #'identity)
        &aux (test (coerce test 'function)) (key (coerce key 'function)))
  (dotree (var tree)
    (when (funcall test target (funcall key var))
      (return var))))

(defun traverse (function tree &aux (function (coerce function 'function)))
  (labels ((rec (tree)
             (funcall function tree)
             (unless (atom tree)
               (catch 'traverse (rec (car tree)))
               (catch 'traverse (rec (cdr tree))))))
    (rec tree)))

(defun find-leaf-if
       (pred tree
        &key (key #'identity)
        &aux (pred (coerce pred 'function)) (key (coerce key 'function)))
  (dotree (var tree)
    (when (funcall pred (funcall key var))
      (return var))))

(defun find-node-if
       (pred tree
        &key (count 1) recursive-p
        &aux (pred (coerce pred 'function)))
  (declare (type (integer 0 #.most-positive-fixnum) count))
  #+(or allegro ccl abcl)
  (check-type count (mod #.most-positive-fixnum))
  (traverse
    (lambda (node)
      (when (and (listp node)
                 (funcall pred node)
                 (cond ((zerop (decf count)) t)
                       (recursive-p nil)
                       (t (throw 'traverse nil))))
        (locally
         (declare (optimize (speed 1)))
         (return-from find-node-if node))))
    tree))

(defun path-to
       (item tree &key (test #'eql) &aux (test (coerce test 'function)))
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
      (reduce (lambda (tree path) (funcall (coerce path 'function) tree)) path
              :initial-value tree)))