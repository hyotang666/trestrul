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
    #:snsubst-if
    ;;;; types
    #:tree
    #:proper-tree
    ;;;; predicates
    #:treep
    #:proper-treep
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

(defun mapleaf(fun tree)
  (check-type tree tree)
  (labels((REC(tree)
	    (cond
	      ((null tree)tree)
	      ((atom tree)(funcall fun tree))
	      (t (cons (REC (car tree))
		       (REC (cdr tree)))))))
    (REC tree)))

(defun nmapleaf(fun tree)
  (check-type tree tree)
  (labels((REC(tree)
	    (cond
	      ((null tree)tree)
	      ((atom tree)(funcall fun tree))
	      (t (rplaca tree (REC (car tree)))
		 (rplacd tree (REC (cdr tree)))))))
    (REC tree)))

(defun remove-leaf(item tree &key(test #'eql)(key #'identity)(keep t))
  (macrolet((traverse(var keep)
	      (flet((!(form)
		      `(HANDLER-CASE,form
			 (ERROR()(ERROR 'SIMPLE-TYPE-ERROR
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
		   (REC,(!`(CAR ,var))(CDR ,var))))))
    (if keep
      (traverse tree t)
      (traverse tree nil))))

(defun remove-leaf-if(function tree &key(key #'identity)(keep t))
  (macrolet((traverse(keep)
	      (flet((!(form)
		      `(HANDLER-CASE,form
			 (ERROR()(ERROR 'SIMPLE-TYPE-ERROR
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
	  (!(form var node)
	    `(HANDLER-CASE,form
	       (ERROR()(ERROR'SIMPLE-TYPE-ERROR
			 :FORMAT-CONTROL "~S: Variable ~S must bound by type of TREE, but ~S.~%About type TREE, evaluate (DESCRIBE '~S)."
			 :FORMAT-ARGUMENTS(LIST 'DOTREE ',var ,node 'TREE)
			 :EXPECTED-TYPE 'TREE :DATUM ,node))))
	  )
    (let((node (gensym "NODE"))
	 (rest(gensym "REST"))
	 (top (gensym "TOP"))
	 (elt (gensym "ELT")))
      (multiple-value-bind(declares body)(SPLIT-DECLARE body)
	`(PROG*((,node ,tree)
		(,elt ,(! `(CAR ,node) var node))
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

(defun collect-node(target tree &key (key #'identity)(test #'eql)recursive-p)
  (check-type tree tree)
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
