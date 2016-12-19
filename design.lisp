(defpackage :trestrul.spec(:use :cl :trestrul :jingoh))
(in-package :trestrul.spec)

(setup :trestrul)

;;;;
(requirements-about type)

#|
In this context, tree is tree structured list.
empty list is one kind of tree. (i.e. empty tree)
|#
#?(treep '()) => T

#|
proper list is one kind of tree. (i.e. proper tree)
|#
#?(treep '(1 2 3)) => T

#|
dotted list is one kind of tree. (i.e. dotted tree)
|#
#?(treep '(1 2 . 3)) => T

#|
nested list is one kind of tree. (i.e. proper tree)
|#
#?(treep '(1 (2) 3)) => T

#|
other atoms are not tree. (but leaf)
|#
#?(treep #()) => NIL
#?(treep :foo) => NIL

#|
proper-tree is the tree structured list which does not have dotted list.
|#
#?(proper-treep ()) => T

#?(proper-treep '(1 2 3)) => T

#?(proper-treep '(1 2 . 3)) => NIL

#?(proper-treep '(1 (2) 3)) => T

#?(proper-treep '(1 (2 . 3) 4)) => NIL

#?(proper-treep #()) => NIL

#?(proper-treep :foo) => NIL

;;;;
(requirements-about mapleaf)

#|
like CL:MAPCAR, apply function to any leaves of tree.
in this context, leaf is non-nil atom in the list.
|#
#?(mapleaf #'1+ '(1 2 3)) => (2 3 4)
, :test tree-equal

#?(mapleaf #'1+ '(1 2 . 3)) => (2 3 . 4)
, :test tree-equal

#?(mapleaf #'1+ '(1 (2 . 3) 4)) => (2 (3 . 4) 5)
, :test tree-equal

#?(mapleaf #'1+ 2) :signals type-error

#|
like CL:MAPCAR, MAPLEAF is not destructive.
|#
#?(defparameter *tree* (list 1 (cons 2 3) 4)) => *TREE*
, :lazy NIL

#?(mapleaf #'1+ *tree*) => (2 (3 . 4) 5)
, :test tree-equal

#?*tree* => (1 (2 . 3) 4)
, :test tree-equal

#|
Unlike CL:MAPCAR, MAPLEAF does not accept some trees.

Rationale: Trestrul treats list as node.
When trees like below comes, what should happen?
(mapleaf #'+ '(1 2 3) '((1) 2 3))
|#
#?(mapcar #'+ (list 1 2 3)(list 1 2 3)) => (2 4 6)
, :test equal

#?(mapleaf #'+ '(1 2 3) '(1 2 3)) :signals error
, :lazy t

;;;;
(requirements-about nmapleaf)

#|
Works same with MAPLEAF, but destructive.

like CL:MAPCAR, apply function to any leaves of tree.
in this context, leaf is non-nil atom in the list.
|#
#?(nmapleaf #'1+ (list 1 2 3)) => (2 3 4)
, :test tree-equal

#?(nmapleaf #'1+ (list* 1 2 3)) => (2 3 . 4)
, :test tree-equal

#?(nmapleaf #'1+ (list 1 (cons 2 3) 4)) => (2 (3 . 4) 5)
, :test tree-equal

#?(nmapleaf #'1+ 2) :signals type-error

#|
Unlike CL:MAPCAR, NMAPLEAF is destructive.
|#
#?(nmapleaf #'1+ *tree*) => (2 (3 . 4) 5)
, :test tree-equal

#?*tree* => (2 (3 . 4) 5)
, :test tree-equal

#|
Unlike CL:MAPCAR, NMAPLEAF does not accept some trees.
|#
#?(mapcar #'+ (list 1 2 3)(list 1 2 3)) => (2 4 6)
, :test equal

#?(nmapleaf #'+ '(1 2 3) '(1 2 3)) :signals error
, :lazy t

;;;;
(requirements-about remove-leaf)

#|
like CL:REMOVE, but works for proper tree.
|#
#?(remove-leaf 1 '(1 (2 3) 4)) => ((2 3) 4)
, :test tree-equal
#?(remove-leaf 1 '(1 (2 . 3) 4)) :signals SIMPLE-TYPE-ERROR

#|
like CL:REMOVE, accept :TEST parameter.
|#
#?(remove-leaf "foo" '("bar" ("foo")"hoge") :test #'string=)
=> ("bar" () "hoge")
, :test equal

#|
like CL:REMOVE, accpet :KEY parameter.
NOTE! - Unlike CL:REMOVE, :KEY function is applied to non-NIL atom only.
        Don't confused!
|#
#?(remove-leaf #\b '("foo" ("bar") "bazz") :key (lambda(x)(char x 0)))
=> ("foo" ())
, :test equal

#|
additionally, REMOVE-LEAF accepts :KEEP parameter.
If it is specified true, tree structure is kept.
if it is specified NIL, empty node (i.e. NIL) is removed.
The default is T.
|#
#?(remove-leaf #\b '("foo" ("bar") "bazz") :key (lambda(x)(char x 0))
		:keep nil)
=> ("foo")
, :test equal

#|
Unlike MAPLEAF and like CL:REMOVE, REMOVE-LEAF accepts only proper tree.
|#
#?(remove-leaf 2 '(1 (2 . 3) 4)) :signals error

#|
like CL:REMOVE, REMOVE-LEAF is works non-destructively.
|#
#?(setf *tree* '(1 (2 3) 4)) => (1 (2 3) 4)
, :test tree-equal

#?(remove-leaf 2 *tree*) => (1 (3) 4)
, :test tree-equal

#?*tree* => (1 (2 3) 4)
, :test tree-equal

;;;;
(requirements-about remove-leaf-if)
#|
Like CL:REMOVE-IF, REMOVE-LEAF-IF accepts predicate as its first argument,
and remove leaves which satisfies predicate.
|#
#?(remove-leaf-if #'evenp '(1 2 (3 4) 5)) => (1 (3) 5)
, :test tree-equal

#|
Like CL:REMOVE-IF, REMOVE-LEAF-IF accepts :key keyword parameter.
|#
#?(remove-leaf-if #'evenp '("1" "2" ("3" "4") "5")
		  :key #'parse-integer)
=> ("1" ("3") "5")
, :test equal

#|
Same with REMOVE-LEAF, REMOVE-LEAF-IF accepts :keep keyword parameter.
The default is T.
|#
#?(remove-leaf-if #'symbolp '(1 2(:foo :bar)("hoge")))
=> (1 2 () ("hoge"))
, :test equal

#?(remove-leaf-if #'symbolp '(1 2(:foo :bar)("hoge"))
		  :keep NIL)
=> (1 2 ("hoge"))
, :test equal

;;;;
(requirements-about dotree)

#|
Like CL:DOLIST, DOTREE iterates over element of tree.
|#
#?(dotree(l '((1 2)(3 . 4)))(princ l))
:outputs "1234"

#|
Like CL:DOLIST, DOTREE can have optional return value.
|#
#?(let(acc)
    (dotree(l '((1 2)(3 . 4)) acc)
      (push l acc)))
=> (4 3 2 1)
, :test equal

#|
Like CL:DOLIST, DOTREE's default return value is NIL.
|#
#?(let((*standard-output*(make-broadcast-stream)))
    (dotree(l '((1 2)(3 . 4)))
      (princ l)))
=> NIL

#|
Like CL:DOLIST, you can RETURN from body.
|#
#?(dotree(l '((1 2)(3 . 4)) :false)
    (when(= l 3)
      (return l)))
=> 3

#|
Like CL:DOLIST, you can GO in the body.
|#
#?(dotree(l '((1 2)(3 . 4)))
    (when(= l 3)
      (go end))
    (princ l)
    end)
:outputs "124"

#|
Like CL:DOLIST, when accept NIL as tree, just return form is evaluated.
|#
#?(dotree(l () :hoge)(print l))
=> :hoge

#|
Like CL:DOLIST, VAR can be refferenced from RETURN form.
|#
#?(dotree(l '(1 2 3)(princ l))(princ l))
:outputs "123NIL"

#|
Unlike CL:DOLIST, DOTREE accepts dotted list as TREE.
|#
#?(dotree(l '(1 2 3 . 4))(princ l))
:outputs "1234"

#|
When tree generate form does not generate tree, an error of type TYPE-ERROR is signaled.
|#
#?(dotree(l (make-symbol "hoge"))(print l)) :signals TYPE-ERROR

;;;;
(requirements-about collect-node)

#|
Like CL:REMOVE-IF-NOT, COLLECT-NODE collect TARGET-node from TREE.
|#
#?(collect-node '(:a) (list(list :a)(list :b)(list :a)(list :c)))
=> NIL

#|
Like CL:REMOVE-IF-NOT, accept :test keyword parameter.
The default is CL:EQL, so above example is failed.
|#
#?(collect-node '(:a) '((:a)(:b)(:a)(:c)) :test #'equal)
=> ((:A)(:A))
, :test equal

#|
Like CL:REMOVE-IF-NOT, accept :key keyword parameter.
The default is CL:IDENTITY.
So example below is equivalent above.
|#
#?(collect-node :a '((:a)(:b)(:a)(:c)) :key #'car)
=> ((:A)(:A))
, :test equal

#|
Can handle dotted-tree.
:key and :test are called only node.
|#
#?(collect-node :a '((:a . :b)(:a) . :c) :key #'car)
=> ((:A . :B)(:A))
, :test equal
#?(collect-node :b '((:a . :b)(:a) . :c) :key #'cdr)
=> ((:A . :B))
, :test equal

#|
COLLECT-NODE accepts :recursive-p keyword parameter.
If it is true, behaves recursively.
The default is NIL.
|#
#?(collect-node :a '((:a (:a)) :b (:c)) :key #'car)
=> ((:A (:A)))
, :test equal
#?(collect-node :a '((:a (:a)) :b (:c)) :key #'car :recursive-p t)
=> ((:A (:A))(:A))
, :test equal

;;;;
(requirements-about asubst)

#|
Like CL:SUBST, but use substituter function.
|#
#?(asubst (constantly 5) nil '(:a))
=> (:A . 5)
, :test equal
#?(asubst (constantly 5) :a '((:a)(:b . :c)((:a :d))))
=> ((5)(:B . :C)((5 :D)))
, :test equal

#|
Unlike other TRESTRUL operators, ASUBST accepts leaf.
This behavior is same with CL:SUBST.
|#
#?(asubst (constantly 5) :a :a)
=> 5

#|
Unlike CL:SUBST, asubst can reference old value as substituter's argument.
Asubst stands on "Anaphoric SUBSTitute".
|#
#?(asubst (lambda(old)(cons :a old))
	  1
	  '((1)(2 . 3)((4) . 1)))
=> (((:A . 1))(2 . 3)((4) :A . 1))
, :test equal

#|
Like CL:SUBST, ASUBST accepts :key keyword parameter.
NOTE - :key applied leaf and node.
|#
#?(asubst (constantly 5) :a '((:a) (:b . :c) ((:a :d)))
	  :key #'car)
:signals error ; (car :b) evaluated internally.

#|
Like CL:SUBST, ASUBST accepts :test keyword parameter.
The default is CL:EQL
|#
#?(asubst (constantly 5) "foo" (format nil "foo"))
=> "foo"
, :test equal
#?(asubst (constantly 5) "foo" "foo" :test #'equal)
=> 5

#|
Unlike CL:SUBST, ASUBST does not accept :test-not keyword parameter.
|#
#?(asubst (constantly 5) "FOO" '(:a :b :c)
	  :test-not #'string=)
:signals error
, :lazy T

;;;;
(requirements-about asubst-if)

#|
Like CL:SUBST-IF, but use substituter function.
|#

#?(asubst-if (constantly 5) #'symbolp '((a)(1 . 2) "string"))
=> ((5 . 5)(1 . 2) "string" . 5)
, :test equal
#?(asubst-if (constantly 5)
	     (lambda(x)(typep x '(cons (eql :a)T)))
	     '((:a) (:b . :c) ((:a :d))))
=> (5 (:b . :c) (5))
, :test equal

#|
Unlike CL:SUBST-IF, ASUBST-IF can reference old value as substituter's argument.
|#

(eval-when(:compile-toplevel :load-toplevel)
  (defstruct foo bar)
  (defmethod make-load-form((s foo)&optional environment)
    (make-load-form-saving-slots s :environment environment))
  )

#?(asubst-if (lambda(x)(apply #'make-foo (cdr x)))
	     (lambda(x)(typep x '(CONS (EQL :FOO) T)))
	     '(defsomething (any lambda list)
		((:hoge "fuga")
		 (:foo :bar "value"))
		(:documentation "doc")))
=> (DEFSOMETHING (ANY LAMBDA LIST)
     ((:HOGE "fuga")
      #S(FOO :BAR "value"))
     (:DOCUMENTATION "doc"))
, :test equalp

;;;;
(requirements-about ansubst)

#|
Like CL:NSUBST, but use substituter function.
|#
#?(ansubst (constantly 5) :a :a)
=> 5

#|
Like CL:NSUBST, tree is destructively modified.
|#
#?(let((tree (list :a :b :c)))
    (ansubst (constantly 5) :a tree)
    tree)
=> (5 :B :C)
, :test equal

