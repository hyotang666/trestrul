(in-package :cl-user)
(defpackage :trestrul (:use :cl)
  (:export
    ;;;; main api - tree functions
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
    #:invalid-tree
    ))
(in-package :trestrul)

(deftype tree()
  "Tree structured list which includes NIL and dotted pair."
  'LIST)

(defun treep(arg)
  (listp arg))

(deftype proper-tree()
  "Tree structured list but without dotted pair."
  '(SATISFIES PROPER-TREEP))

(defun proper-treep(arg)
  (labels((REC(arg)
	    (typecase arg
	      (NULL t)
	      (ATOM (return-from proper-treep nil))
	      (t (BODY(car arg)(cdr arg)))))
	  (BODY(first rest)
	    (when(consp first)
	      (REC first))
	    (REC rest)))
    (REC arg)))

(define-condition invalid-tree(simple-type-error)())

(deftype function-designator()
  '(or function (and symbol (not(or boolean keyword)))))

(defun ensure-function(form)
  (typecase form
    ((cons(eql quote)(cons symbol null))
     `#',(cadr form))
    ((or (cons(eql function)t)
	 (cons(eql lambda)t))
     form)
    (t `(coerce ,form 'function))))

;;;; MAPLEAF
(declaim(ftype (function (function-designator tree)
			 (values tree &optional))
	       map-leaf))

(defun mapleaf(fun tree)
  (check-type tree tree)
  (%mapleaf (coerce fun 'function) tree))

(define-compiler-macro mapleaf(fun tree)
  `(%mapleaf ,(ensure-function fun)
	     ,tree))

(declaim(ftype (function(function tree)
		 (values tree &optional))
	       %mapleaf))

(defun %mapleaf(fun tree)
  (declare (type function fun)
	   (type tree tree))
  (labels((REC(tree)
	    (cond
	      ((null tree)tree)
	      ((atom tree)(values(funcall fun tree)))
	      (t (cons (REC (car tree))
		       (REC (cdr tree)))))))
    (REC tree)))

;;;; NMAPLEAF
(declaim(ftype (function (function-designator tree)
			 (values tree &optional))
	       nmapleaf))

(defun nmapleaf(fun tree)
  (assert (typep tree 'tree))
  (%nmapleaf (coerce fun 'function)tree))

(define-compiler-macro nmapleaf(fun tree)
  `(%nmapleaf ,(ensure-function fun)
	      ,tree))

(declaim (ftype (function (function tree)
			  (values tree &optional))
		%nmapleaf))

(defun %nmapleaf(fun tree)
  (declare (type function fun)
	   (optimize(speed 3))
	   (type tree tree))
  (labels((REC(tree)
	    (cond
	      ((null tree)tree)
	      ((atom tree)(values(funcall fun tree)))
	      (t (rplaca tree (REC (car tree)))
		 (rplacd tree (REC (cdr tree)))))))
    (REC tree)))

(macrolet((check(form type api)
	    (let((datum(gensym "DATUM")))
	      `(LET((,datum ,form))
		 (ASSERT (TYPEP ,datum ',type)()
			 'INVALID-TREE
			 :FORMAT-CONTROL "~S: Must tree but ~S"
			 :FORMAT-ARGUMENTS (LIST ',api ,datum)
			 :EXPECTED-TYPE 'TREE
			 :DATUM ,datum)))))

  (defun collect-node(target tree &key (key #'identity)(test #'eql)recursive-p)
    (check tree tree collect-node)
    (macrolet((expand(rec-p)
		`(LABELS((REC(TREE)
			   (ETYPECASE TREE
				      (NULL ; END OF PROPER LIST
					(WHEN(FUNCALL TEST TARGET (FUNCALL KEY TREE))
					  (PUSH TREE ACC)))
				      (ATOM ; END OF DOTTED LIST
					NIL) ; ignore leaf.
				      (CONS ; PROGRESS
					(BODY(CAR TREE)(CDR TREE)))))
			 (BODY(ELT REST)
			   (IF(LISTP ELT)
			     (ELT-IS-NODE ELT REST)
			     (REC REST))) ; ignore leaf.
			 (ELT-IS-NODE(NODE REST)
			   (IF(FUNCALL TEST TARGET(FUNCALL KEY NODE))
			     (TARGET-IS-FOUND NODE REST)
			     (PROGN (REC NODE)
				    (REC REST))))
			 (TARGET-IS-FOUND(FOUND REST)
			   ,(if (not rec-p)
			      `(PROGN (PUSH FOUND ACC)
				      (REC REST))
			      `(PROGN (PUSH FOUND ACC)
				      (WHEN FOUND
					    (REC FOUND))
				      (REC REST))))
			 )
		   (REC TREE))))
      (let(acc)
	(if recursive-p
	  (expand t)
	  (expand nil))
	(nreverse acc))))
  )

(defun remove-leaf(item tree &key(test #'eql)(key #'identity)(keep t))
  (macrolet((traverse(keep)
	      (flet((!(form)
		      `(HANDLER-CASE,form
			 (ERROR()(ERROR 'INVALID-TREE
					:FORMAT-CONTROL "REMOVE-LEAF accepts only proper tree, but ~S.~&About PROPER-TREE, evaluate (describe '~S)."
					:FORMAT-ARGUMENTS (LIST TREE 'PROPER-TREE)
					:EXPECTED-TYPE 'PROPER-TREE
					:DATUM TREE))))
		    (may-push(keep)
		      (if keep
			`(PUSH (REC(CAR FIRST)(CDR FIRST))
			       ACC)
			`(LET((IT(REC(CAR FIRST)(CDR FIRST))))
			   (IF IT
			      (PUSH IT ACC)
			      ACC)))))
		`(LABELS((REC(FIRST REST &OPTIONAL ACC)
			   (ETYPECASE FIRST
			      (CONS (REC (CAR REST)(CDR REST)
					 ,(may-push keep)))
			      (NULL (NREVERSE ACC))
			      (ATOM (REC ,(!`(CAR REST))(CDR REST)
					 (IF(FUNCALL TEST ITEM(FUNCALL KEY FIRST))
					   ACC
					   (PUSH FIRST ACC)))))))
		   (REC,(!`(CAR TREE))(CDR TREE))))))
    (if keep
      (traverse t)
      (traverse nil))))

;;;; REMOVE-LEAF-IF
(declaim (ftype (function ((or symbol function)
			   proper-tree
			   &key
			   (:key (or symbol function))
			   (:keep boolean))
			  (values proper-tree &optional))
		remove-leaf-if))

(defun remove-leaf-if(function tree &key(key #'identity)(keep t))
  (%remove-leaf-if (coerce function 'function)
		   tree
		   (coerce key 'function)
		   keep))

(define-compiler-macro remove-leaf-if (function tree &key (key '#'identity)(keep t))
  `(%remove-leaf-if ,(ensure-function function)
		    ,tree
		    ,(ensure-function key)
		    ,keep))

(declaim (ftype (function (function proper-tree function boolean)
			  (values proper-tree &optional))
		%remove-leaf-if))

(defun %remove-leaf-if(function tree key keep)
  (declare (type function function key)
	   (optimize speed))
  (macrolet((traverse(keep)
	      (flet((!(form)
		      `(HANDLER-CASE,form
			 (ERROR()(ERROR 'INVALID-TREE
					:FORMAT-CONTROL "REMOVE-LEAF-IF accepts only proper tree, but ~S.~&About PROPER-TREE, evaluate (describe '~S)."
					:FORMAT-ARGUMENTS (LIST TREE 'PROPER-TREE)
					:EXPECTED-TYPE 'PROPER-TREE
					:DATUM TREE))))
		    (may-push(keep)
		      (if keep
			`(PUSH (REC(CAR FIRST)(CDR FIRST))
			       ACC)
			`(LET((IT(REC(CAR FIRST)(CDR FIRST))))
			   (IF IT
			       (PUSH IT ACC)
			       ACC)))))
		`(LABELS((REC(FIRST REST &OPTIONAL ACC)
			   (ETYPECASE FIRST
	                     (CONS (REC (CAR REST)(CDR REST)
					,(may-push keep)))
			     (NULL (NREVERSE ACC))
			     (ATOM (REC ,(!`(CAR REST))(CDR REST)
					(IF(FUNCALL FUNCTION(FUNCALL KEY FIRST))
					  ACC
					  (PUSH FIRST ACC)))))))
		   (REC ,(!`(CAR TREE))(CDR TREE))))))
    (if keep
      (traverse t)
      (traverse nil))))

(defmacro dotree((var tree &optional return)&body body)
  (labels((SPLIT-DECLARE(body)
	    (loop :for sexp :in body
		  :when (typep sexp '(CONS (EQL DECLARE) T))
		  :collect sexp :into declares
		  :else :collect sexp :into bodies
		  :finally (return (values declares bodies))))
	  (!(form node)
	    `(HANDLER-CASE,form
	       (ERROR()(ERROR'INVALID-TREE
			 :FORMAT-CONTROL "~S: Form TREE must generate TREE, but ~S.~%About type TREE, evaluate (DESCRIBE '~S)."
			 :FORMAT-ARGUMENTS(LIST 'DOTREE ,node 'TREE)
			 :EXPECTED-TYPE 'TREE :DATUM ,node))))
	  )
    (let((node (gensym "NODE"))
	 (rest(gensym "REST"))
	 (top (gensym "TOP"))
	 (elt (gensym "ELT")))
      (multiple-value-bind(declares body)(SPLIT-DECLARE body)
	`(PROG*((,node ,tree)
		(,elt ,(! `(CAR ,node) node))
		(,rest (CDR ,node)))
	   ,top
	   (IF(AND(NULL ,rest)(NULL ,elt))
	     (RETURN (LET(,var)
		       (DECLARE(IGNORABLE ,var))
		       ,return))
	     (IF(CONSP ,elt)
	       (SETF ,rest (CONS (CDR ,elt) ,rest)
		     ,elt (CAR ,elt))
	       (PROGN (WHEN,elt
			(LET((,var ,elt))
			  ,@declares
			  (TAGBODY ,@body)))
		      (IF(AND ,rest (ATOM ,rest))
			(SHIFTF ,elt ,rest NIL)
			(SETF ,elt (CAR ,rest)
			      ,rest (CDR ,rest))))))
	   (GO ,top))))))

(defun asubst(substituter target tree &key(test #'eql)(key #'identity))
  (if(funcall test target (funcall key tree))
    (funcall substituter tree)
    (if(atom tree)
      tree
      (cons (asubst substituter target (car tree) :test test :key key)
	    (asubst substituter target (cdr tree) :test test :key key)))))

(defun asubst-if(substituter predicate tree &key (key #'identity))
  (if(funcall predicate (funcall key tree))
    (funcall substituter tree)
    (if(atom tree)
      tree
      (cons (asubst-if substituter predicate (car tree) :key key)
	    (asubst-if substituter predicate (cdr tree) :key key)))))

(defun ansubst(substituter target tree &key(test #'eql)(key #'identity))
  (if(funcall test target (funcall key tree))
    (funcall substituter tree)
    (if(atom tree)
      tree
      (progn (rplaca tree (ansubst substituter target (car tree) :test test :key key))
	     (rplacd tree (ansubst substituter target (cdr tree) :test test :key key))))))

(defun ansubst-if(substituter predicate tree &key (key #'identity))
  (if(funcall predicate (funcall key tree))
    (funcall substituter tree)
    (if(atom tree)
      tree
      (progn (rplaca tree (ansubst-if substituter predicate (car tree) :key key))
	     (rplacd tree (ansubst-if substituter predicate (cdr tree) :key key))))))

(defun find-leaf(target tree &key (test #'eql)(key #'identity))
  (dotree(var tree)
    (when(funcall test target (funcall key var))
      (return var))))

(defun traverse(function tree)
  (funcall function tree)
  (unless(atom tree)
    (catch 'traverse
	   (traverse function (car tree)))
    (catch 'traverse
	   (traverse function (cdr tree)))))

(defun find-leaf-if(pred tree &key (key #'identity))
  (dotree(var tree)
    (when(funcall pred (funcall key var))
      (return var))))

(defun find-node-if(pred tree &key (count 1)recursive-p)
  (check-type count (integer 1 *))
  (traverse (lambda(node)
	      (when(and (listp node)
			(funcall pred node)
			(cond
			  ((zerop (decf count)) t)
			  (recursive-p nil)
			  (t (throw 'traverse nil))))
		(return-from find-node-if node)))
	    tree))

(defun path-to(item tree &key (test #'eql))
  (labels((rec(tree &optional path)
	    (when(funcall test item tree)
	      (return-from path-to (values (nreverse path) T)))
	    (if(atom tree)
	      (values nil nil)
	      (progn (rec (car tree) (cons 'car path))
		     (rec (cdr tree) (cons 'cdr path))))))
    (rec tree)))

(defun follow (path tree)
  (if(null path)
    tree
    (reduce (lambda(tree path)
	      (funcall path tree))
	    path
	    :initial-value tree)))
