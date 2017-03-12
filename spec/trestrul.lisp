(defpackage :trestrul.spec (:use :cl :jingoh :trestrul))
(in-package :trestrul.spec)
(setup :trestrul.spec)

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
#?(op #'1+ 1) :signals type-error

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
#?(remove-leaf 1 '(1 2 3 (4)))
=> (2 3 (4))
,:test tree-equal

#+syntax
(REMOVE-LEAF item tree &key (test #'eql) (key #'identity) (keep t)) ; => result

;;;; Arguments and Values:

; item := non NIL atom, otherwise unspecified.
#?(remove-leaf () '(()1 2)) => unspecified

; tree := tree structured list, otherwise error.
#?(remove-leaf 1 1) :signals type-error

; test := function-designator which designate the function which accepts two arguments.
; Otherwise error.
; The default is CL:EQL.
#?(remove-leaf "1" '("2" "1" "0"))
=> ("2" "1" "0")
,:test equal
#?(remove-leaf "1" '("2" "1" "0") :test 'string=)
=> ("2" "0")
,:test equal
#?(remove-leaf "1" '("2" "1" "0") :test :not-function-designator)
:signals error

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
:signals error

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
:signals error

; tree := tree structured list, otherwise error.
#?(remove-leaf-if #'evenp 0)
:signals type-error

; key := function-desinator which designates the function which accepts one argument.
#?(remove-if #'evenp '("1" "2" "3"))
:signals error
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
:signals type-error

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
#?(collect-node :hoge :not-tree) :signals error

; key := function designator which designates the function which accepts one node as argument.
; The default is CL:IDENTITY.
; If it is not function designator, an error is signaled.
#?(collect-node :use '((:use :cl)(:export :a :b :c))
		:key :not-function-designator)
:signals error

; test := function designator which designates the function which accepts two arguments.
; The default is CL:EQL.
; if it is not function-designator, an error is signaled.
#?(collect-node '(:use :cl) '((:use :cl)(:export :a :b :c))
		:test :not-function-designator)
:signals error

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
; Substitute leaf with applying substituter.
#?(op #'princ-to-string 1 '(1 2 3 1 2 3))
=> ("1" 2 3 "1" 2 3)
,:test equal

#+syntax
(OP substituter target tree &key (test #'eql) (key #'identity)) ; => result

;;;; Arguments and Values:

; substituter := function-designator which designates the function which accepts one argument.
; If it is not function-designator, an error is signaled.
#?(op :not-function-designator 1 '(1 2 3))
:signals error

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
:signals error

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
:signals error

; predicate := function designator which desigates the function which accepts one argument.
; if it is not funciton designator, an error is signaled.
#?(asubst-if #'identity :not-function-designator '(1 2 3))
:signals error

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
:signals error

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

