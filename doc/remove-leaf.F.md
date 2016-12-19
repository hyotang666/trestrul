# [Function] REMOVE-LEAF

## Syntax:

(REMOVE-LEAF item tree &key (test #'eql) (key #'identity) (keep t)) => result

## Arguments and Values:

item := any lisp object

tree := tree

test := function designator

key := function designator

keep := boolean

result := tree

## Description:
Like CL:REMOVE but recursively.

When keep is NIL, empty node (i.e. NIL) is also removed.

## Example:

## Affected-By:

## Side-Effects:

## Notes:

## See-Also:
REMOVE-LEAVES PROPER-TREE

## Exceptional-Situations:
When TREE is not proper-tree, an error will be signaled.

