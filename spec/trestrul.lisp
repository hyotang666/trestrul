(defpackage :trestrul.spec (:use :cl :jingoh :trestrul))
(in-package :trestrul.spec)
(setup :trestrul)

(common-requirements-about (mapleaf nmapleaf)
			   :as op)

#+syntax
(OP fun tree) ; => result

;;;; Description:
; Apply FUN to every leaf of TREE.
#?(op #'1+ '(1 (2 . 3) . 4))
=> (2 (3 . 4) . 5)
,:test tree-equal

;;;; Arguments and Values:

; fun := function which accepts one argument. Otherwise error.
#?(op 0 '(1 2 3 4)) :signals error

; tree := tree structured list. Otherwise error.
#?(op #'1+ 1) :signals invalid-tree

; result := see below.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MAPLEAF)

; result := newly consed tree structured list.
#?(let((tree'(1 . 2)))
    (eq tree (mapleaf #'identity tree)))
=> NIL

(requirements-about NMAPLEAF)

;;;; Side-Effects:
; argument TREE is destructively modified.
#?(let((tree '(1 2 3)))
    (eq tree (nmapleaf #'1+ tree)))
=> T

(requirements-about REMOVE-LEAF)

;;;; Description:
; remove specified LEAF from TREE.
#?(remove-leaf 4 '(1 2 3 (4)))
=> (1 2 3 ())
,:test tree-equal

#+syntax
(REMOVE-LEAF item tree &key (test #'eql) (key #'identity) (keep t)) ; => result

;;;; Arguments and Values:

; item := non NIL atom, otherwise unspecified.
#?(remove-leaf () '(()1 2)) => unspecified

; tree := tree structured list, otherwise error.
#?(remove-leaf 1 1) :signals invalid-tree

; test := function-designator which designate the function which accepts two arguments.
; Otherwise error.
; The default is CL:EQL.
#?(remove-leaf (princ-to-string 1) (list "2" "1" "0"))
=> ("2" "1" "0")
,:test equal
#?(remove-leaf "1" '("2" "1" "0") :test 'string=)
=> ("2" "0")
,:test equal
#?(remove-leaf "1" '("2" "1" "0") :test :not-function-designator)
:signals undefined-function

; key := function-designator which designates the function which accepts one argument.
; Otherwise error.
; The default is CL:IDENTITY.
#?(remove-leaf 1 '("2" "1" "0"))
=> ("2" "1" "0")
,:test equal
#?(remove-leaf 1 '("2" "1" "0"):key #'parse-integer)
=> ("2" "0")
,:test equal
#?(remove-leaf "1" '("2" "1" "0") :key :not-function-designator)
:signals undefined-function

; keep := boolean
; control flag, whether keep empty node (i.e. NIL) or not.
; The default it T.
#?(remove-leaf 1 '(1 2 (1) 2))
=> (2 () 2)
,:test tree-equal
#?(remove-leaf 1 '(1 2 (1) 2) :keep nil)
=> (2 2)
,:test tree-equal

; result := newly consed tree structured list.
#?(let((tree '(1 2 3 4 5)))
    (eq tree (remove-leaf 0 tree)))
=> NIL

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; TREE must be proper tree. See PROPER-TREE for detail.

;;;; Exceptional-Situations:

(requirements-about REMOVE-LEAF-IF)

;;;; Description:
; Remove leaf which satisfies FUNCTION.
#?(remove-leaf-if #'evenp '(1 (2 3) 4))
=> (1 (3))
,:test tree-equal

#+syntax
(REMOVE-LEAF-IF function tree &key (key #'identity) (keep t)) ; => result

;;;; Arguments and Values:

; function := function-designator which designates the function which accepts one argument.
; Otherwise error.
#?(remove-leaf-if 'oddp '(1 (2 3) 4))
=> ((2)4)
,:test tree-equal
#?(remove-leaf-if :not-function-designator '(1 (2 3) 4))
:signals undefined-function

; tree := tree structured list, otherwise error.
#?(remove-leaf-if #'evenp 0)
:signals invalid-tree

; key := function-desinator which designates the function which accepts one argument.
#?(remove-if #'evenp '("1" "2" "3"))
:signals type-error
#?(remove-if #'evenp '("1" "2" "3") :key #'parse-integer)
=> ("1" "3")
,:test equal

; keep := boolean which control whether keep empty node or not.
; The default is T.
#?(remove-leaf-if #'evenp '(1 (2) 3))
=> (1 () 3)
,:test tree-equal
#?(remove-leaf-if #'evenp '(1 (2) 3) :keep nil)
=> (1 3)
,:test tree-equal

; result := newly consed tree structured list.
#?(let((tree '(1 2 3)))
    (eq tree (remove-leaf-if #'evenp tree)))
=> NIL

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DOTREE)

;;;; Description:
; iterate body with var bound by each leaf of TREE.
#?(dotree(l '(1 2 . 3))
    (princ l))
:outputs "123"

#+syntax
(DOTREE (var tree &optional return) &body body) ; => result

;;;; Arguments and Values:

; var := symbol which bound by each leaf. otherwise error.
#?(dotree(0 '(1 2 . 3))
    (princ 0))
:signals error

; tree := tree generate form. Not evaluated.
; When the form does not generate tree structured list,
; an error is signaled.
#?(dotree(l 'tree)
    (princ l))
:signals invalid-tree
,:ignore-signals warning

; return := return form which evaluated only once after iteration.
#?(dotree(l '(1 2 . 3) :result)
    (princ l))
=> :RESULT
,:stream NIL

; body := implicit progn

; result := return value of RETURN. The default is nil.
#?(dotree(l '(1 2 . 3))
    (1+ l))
=> NIL

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; can declare about VAR at top of BODY.
#?(dotree(l '(1 2 . 3))
    (declare(type fixnum l))
    (princ l))
:outputs "123"

; VAR is able to see from RETURN form,
; but VAR is bound NIL in such time.
#?(dotree(l '(1 2 . 3)(princ l))
    (1+ l))
:outputs "NIL"

; BLOCK named NIL is implicitly established.
; So RETURN is able to be used.
#?(dotree(l '(1 2 3 4))
    (if(= 3 l)
      (return)
      (princ l)))
:outputs "12"

; When empty tree comes, only return form is evaluated.
#?(dotree(l () :hoge)
    (princ l))
=> :hoge

; When CL:RETURN is called, RETURN form is not evaluated.
#?(dotree(l '(1 2 3 4)(princ l))
    (if(= l 3)
      (return)
      (princ l)))
:outputs "12"

; TAGBODY is implicitly established.
; So GO is able to be used in body.
#?(dotree(l '(1 2 3 4))
    (if(= 3 l)
      (go 3)
      (go :end))
    3
    (princ l)
    :end
    (princ l))
:outputs "12334"

;;;; Exceptional-Situations:

(requirements-about COLLECT-NODE)

;;;; Description:
; collect node from TREE.
#?(collect-node :export `(defpackage :hoge (:use :cl)
			       (:export :a :b :c)
			       (:export :d :e :f))
		:key #'car)
=> ((:export :a :b :c)(:export :d :e :f))
,:test equal

#+syntax
(COLLECT-NODE target tree &key (key #'identity) (test #'eql) recursive-p) ; => result

;;;; Arguments and Values:

; target := T

; tree := tree structured list. Otherwise error.
#?(collect-node :hoge :not-tree) :signals invalid-tree

; key := function designator which designates the function which accepts one node as argument.
; The default is CL:IDENTITY.
; If it is not function designator, an error is signaled.
#?(collect-node :use '((:use :cl)(:export :a :b :c))
		:key :not-function-designator)
:signals undefined-function

; test := function designator which designates the function which accepts two arguments.
; The default is CL:EQL.
; if it is not function-designator, an error is signaled.
#?(collect-node '(:use :cl) '((:use :cl)(:export :a :b :c))
		:test :not-function-designator)
:signals undefined-function

; recursive-p := boolean which control whether call collect-node recursively or not.
#?(collect-node :tag '((:tag recursively (:tag)))
		:key #'car)
=> ((:tag recursively (:tag)))
,:test equal
#?(collect-node :tag '((:tag recursively (:tag second)))
		:key #'car
		:recursive-p T)
=> ((:tag recursively (:tag second))
    (:tag second))
,:test equal

; result := list which may contains node.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; When both :key and :test are not specified,
; return form is always nil unless target is nil.
#?(collect-node :tag '((:tag 1 (:tag 2))))
=> NIL

#?(collect-node nil '((:tag 1 ()(:tag 2 ()))))
=> (NIL NIL NIL NIL NIL)
;; i.e. ((:tag 1 NIL (:tag 2 NIL . NIL) . NIL) . NIL)
,:test equal
; Works fine even if :recirsive-p T is specified (i.e. never infinite loop).
#?(collect-node nil '((:tag 1 NIL(:tag 2 NIL)))
		:recursive-p T)
=> (NIL NIL NIL NIL NIL)
,:test equal

#?(collect-node nil '())
=> (NIL) ; remember tree is one kind of node.
,:test equal
#?(collect-node nil '() :recursive-p t)
=> (NIL)
,:test equal

#?(collect-node nil '(NIL))
=> (NIL NIL) ; remember (NIL) is (NIL . NIL)
,:test equal
#?(collect-node nil '(NIL) :recursive-p t)
=> (NIL NIL)
,:test equal

;;;; Exceptional-Situations:

(common-requirements-about (ASUBST ANSUBST)
			   :as op)

;;;; Description:
; Substitute tree with applying substituter.
#?(op #'princ-to-string 1 '(1 2 3 1 2 3))
=> ("1" 2 3 "1" 2 3)
,:test equal

#+syntax
(OP substituter target tree &key (test #'eql) (key #'identity)) ; => result

;;;; Arguments and Values:

; substituter := function-designator which designates the function which accepts one argument.
; If it is not function-designator, an error is signaled.
#?(op :not-function-designator 1 '(1 2 3))
:signals (or undefined-function
	     error ; for ccl
	     )

; target := any lisp object.

; tree := tree structured list, or leaf.
#?(op #'princ-to-string :leaf :leaf) => "LEAF"
,:test string=

; test := function designator which designates the function which accepts two arguments to test equality.
#?(op #'parse-integer "0" '("1" "2" "3" "0")
      :test #'equal)
=> ("1" "2" "3" 0)
,:test equal
; If it is not function designator, an error is signaled.
#?(op #'identity 0 '(1 2 3)
	  :test :not-function-designator)
:signals (or undefined-function
	     error ; for ccl
	     )

; key := function designator which designates the function which accepts one argument.
#?(op (constantly :zero) 0 '("1" "2" "3" "0")
	  :key #'(lambda(x)
		   (if(stringp x)
		     (parse-integer x)
		     x)))
=> ("1" "2" "3" :ZERO)
,:test equal

; result := see below.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ASUBST)

; result := may newly consed substituted value.
#?(let((tree '(:a :b :c)))
    (eq tree (asubst (constantly :hoge)
		     :a
		     tree)))
=> NIL

(requirements-about ANSUBST)

; result := may destructively modified substituted value.
#?(let((tree '(:a :b :c)))
    (eq tree (ansubst (constantly :hoge)
		      :a
		      tree)))
=> T

(requirements-about ASUBST-IF)

;;;; Description:
; substitute leaf which satisfies PREDICATE.
#?(asubst-if #'princ-to-string
	     (lambda(x)
	       (unless (listp x)
		 (evenp x)))
	     '(1 2 3 4 5))
=> (1 "2" 3 "4" 5)
,:test equal

#+syntax
(ASUBST-IF substituter predicate tree &key (key #'identity)) ; => result

;;;; Arguments and Values:

; substituter := function designator which designates the function which accepts one argument.
; if it is not function designator, an error is signaled.
#?(asubst-if :not-function-designator
	     (lambda(x)
	       (unless(listp x)
		 (numberp x)))
	     '(1 2 3 4 5))
:signals (or undefined-function
	     error ; for ccl
	     )

; predicate := function designator which desigates the function which accepts one argument.
; if it is not funciton designator, an error is signaled.
#?(asubst-if #'identity :not-function-designator '(1 2 3))
:signals (or undefined-function
	     error ; for ccl
	     )

; tree := leaf or tree structured list.
#?(asubst-if #'symbol-name #'symbolp :hoge)
=> "HOGE"
,:test string=

; key := function designator which designates the function which accepts one argument.
; if it is not function designator, an error is signaled.
#?(asubst-if 'string (complement #'symbol-package)
	     '(hoge :hoge #:hoge "HOGE")
	     :key #'(lambda(x)
		      (and (symbolp x)
			   x)))
=> (hoge :hoge "HOGE" "HOGE")
,:test equal
#?(asubst-if #'string #'null '(hoge :hoge #:hoge nil)
	     :key :not-function-designator)
:signals undefined-function

; result := substituted leaf or tree.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; Top level node (i.e. root) is also tested.
#?(asubst-if (constantly :hoge) #'listp '(:a :b :c))
=> :hoge

;;;; Exceptional-Situations:

(requirements-about TREE)
;;;; Description:
; Tree structured list which includes NIL and dotted pair.
#?(typep nil 'tree) => T
#?(typep '(:a . :b) 'tree) => T
#?(typep :atom 'tree) => NIL

;;;; Compound Type Specifier Kind:
; none

;;;; Compound Type Specifier Syntax:

;;;; Compound Type Specifier Arguments:

;;;; Compound Type Specifier Description:

(requirements-about PROPER-TREE)
;;;; Description:
; Tree structured list but without dotted pair.
#?(typep () 'proper-tree) => T
#?(typep '(:a . :b) 'proper-tree) => NIL
#?(typep '(:a :b) 'proper-tree) => T
#?(typep :atom 'proper-tree) => NIL
#?(typep '((:a :b)) 'proper-tree) => T
#?(typep '((:a . :b) :c) 'proper-tree) => NIL

;;;; Compound Type Specifier Kind:
; none

;;;; Compound Type Specifier Syntax:

;;;; Compound Type Specifier Arguments:

;;;; Compound Type Specifier Description:

(requirements-about TREEP)

;;;; Description:
; Tests arg is tree.
#?(treep :leaf) => NIL
#?(treep ()) => T
#?(treep '(:a :b)) => T
#?(treep '(:a . :b)) => T
#+syntax
(TREEP arg) ; => result

;;;; Arguments and Values:

; arg := any lisp object.

; result := boolean

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about PROPER-TREEP)

;;;; Description:
; Tests arg is proper-tree.
#?(proper-treep :leaf) => NIL
#?(proper-treep ()) => T
#?(proper-treep '(:a :b)) => T
#?(proper-treep '(:a . :b)) => NIL

#+syntax
(PROPER-TREEP arg) ; => result

;;;; Arguments and Values:

; arg := any lisp object

; result := boolean

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about INVALID-TREE)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; invalid-tree simple-type-error simple-condition type-error error serious-condition condition slot-object t

;;;; Effective Slots:

; FORMAT-CONTROL [Type] T
; [READER] simple-condition-format-control

; FORMAT-ARGUMENTS [Type] T
; [READER] simple-condition-format-arguments

; DATUM [Type] T
; [READER] type-error-datum

; EXPECTED-TYPE [Type] T
; [READER] type-error-expected-type

;;;; Notes:

(requirements-about FIND-LEAF)

;;;; Description:
; Find target from tree.

#+syntax
(FIND-LEAF target tree &key (test #'eql) (key #'identity)) ; => result

#?(find-leaf 0 '(1 (2 . 3)((0)))) => 0

;;;; Arguments and Values:

; target := T

; tree := Tree, otherwise error.
#?(find-leaf 0 0) :signals error

; test := Function as `(FUNCTION(T T)BOOLEAN)`, the default is #'EQL.

; key := Function as `(FUNCTION(T)T)`, the default is #'IDENTITY.

; result := T

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; Same as `CL:FIND`, `FIND-LEAF` can not find `NIL`.
#?(find-leaf nil '(())) => NIL

;;;; Exceptional-Situations:

(requirements-about TRAVERSE)

;;;; Description:
; Traverse TREE with doing FUNCTION.
; This is `MAPC` like operator.

#+syntax
(TRAVERSE function tree) ; => result

#?(traverse #'print '(1 (2 . 3) 4))
:outputs "
(1 (2 . 3) 4) 
1 
((2 . 3) 4) 
(2 . 3) 
2 
3 
(4) 
4 
NIL "

;;;; Arguments and Values:

; function := Function as (FUNCTION(T)T).
; Otherwise error.
#?(traverse "NOT-FUNCTION" '(1 2 3))
:signals error
; This function's return value is discarded.

; tree := Tree structured list.
; Atom is acceptable.
#?(traverse #'print :atom)
:outputs "
:ATOM "

; result := NIL
#?(traverse #'print :atom)
=> NIL
,:stream NIL

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; `TRAVERSE` dinamically construct `CATCH` tag named `TRAVERSE`.
; You can use it to skip traversing.
#?(traverse (lambda(x)
	      (if(and (listp x)
		      (eql 2 (car x)))
		(throw 'traverse nil)
		(print x)))
	    '(1 (2 . 3) 4))
:outputs "
(1 (2 . 3) 4) 
1 
((2 . 3) 4) 
(4) 
4 
NIL "

;;;; Exceptional-Situations:

(requirements-about FIND-LEAF-IF)

;;;; Description:
; Find element which satisfies PRED from TREE.

#+syntax
(FIND-LEAF-IF pred tree &key (key #'identity)) ; => result

#?(find-leaf-if #'oddp '(2 (3 . 4) 5))
=> 3
;;;; Arguments and Values:

; pred := function designator as (function(T)T).
#?(find-leaf-if 'oddp '(1 2 3))
=> 1
; otherwise error
#?(find-leaf-if "NOT-FUNCTION" '(1 2 3))
:signals error

; tree := tree structured list.
; atom is not acceptable.
#?(find-leaf-if #'oddp 1) :signals error

; key := function designator as (function(T)T).
#?(find-leaf-if #'oddp '("1" "2" "3") :key #'parse-integer)
=> "1"
,:test equal
#?(find-leaf-if #'oddp '("1" "2" "3") :key 'parse-integer)
=> "1"
,:test equal
; otherwise error.
#?(find-leaf-if #'oddp '("1" "2" "3") :key "NOT-FUNCTION")
:signals error

; result := T

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about FIND-NODE-IF)

;;;; Description:
; Find node which satisfies `PRED` from `TREE`.

#+syntax
(FIND-NODE-IF pred tree &key (count 1) recursive-p) ; => result

;;;; Arguments and Values:

; pred := Function designator as (function (list) boolean), otherwise error.
#?(find-node-if "not function designator" '(:dummy))
:signals error

; tree := Tree, which include atom.
#?(find-node-if #'identity "atom") => NIL

; count := (integer 1 *), otherwise error.
#?(find-node-if #'identity nil :count "not (integer 1 *)")
:signals error
; `COUNT` specify Nth value. The default is 1.
#?(find-node-if (lambda(x)(typep x '(cons (eql :a)*)))
		'((:a 1)(:a 2)(:a 3)))
=> (:a 1)
,:test equal
#?(find-node-if (lambda(x)(typep x '(cons (eql :a)*)))
		'((:a 1)(:a 2)(:a 3))
		:count 2)
=> (:A 2)
,:test equal
#?(find-node-if (lambda(x)(typep x '(cons (eql :a)*)))
		'((:a 1)(:a 2)(:a 3))
		:count 3)
=> (:A 3)
,:test equal
#?(find-node-if (lambda(x)(typep x '(cons (eql :a)*)))
		'((:a 1)(:a 2)(:a 3))
		:count 4)
=> NIL

; recursive-p := Generalized boolean. The default is `NIL`.
#?(find-node-if (lambda(x)(eq :a (car x)))
		'((:a 1 :a 2 :a 3) (:a 4))
		:count 2)
=> (:A 4)
,:test equal
#?(find-node-if (lambda(x)(eq :a (car x)))
		'((:a 1 :a 2 :a 3)(:a 4))
		:count 2
		:recursive-p t)
=> (:A 2 :A 3)
,:test equal

; result := T

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about PATH-TO)

;;;; Description:
; Return list of functions, which guide you to target item in the tree.
; Secondary value is foundp.
#?(path-to 1 1) :values (NIL T)
#?(path-to 1 '(1))
:multiple-value-satisfies #`(& (listp $v1)
			       (= 1 (length $v1))
			       (eq #'car (car $v1))
			       (eql 1 (reduce (lambda(tree path)
						(funcall path tree))
					      $v1
					      :initial-value '(1)))
			       (eq t $v2))
#?(path-to 1 '(2)) :values (NIL NIL)
#?(path-to 2 '(1 . 2))
:multiple-value-satisfies #`(& (listp $v1)
			       (= 1 (length $v1))
			       (eq #'cdr (car $v1))
			       (eql 2 (reduce (lambda(tree path)
						(funcall path tree))
					      $v1
					      :initial-value '(1 . 2)))
			       (eq t $v2))

#+syntax
(PATH-TO item tree &key (test #'eql)) ; => result

;;;; Arguments and Values:

; item := T

; tree := Tree structured list, include atom. i.e. T.

; test := (function(t t)generalized-boolean).

; result := (values (function*) boolean)

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

