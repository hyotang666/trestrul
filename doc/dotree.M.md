# [Macro] DOTREE

## Syntax:

(DOTREE (var tree &optional return) &body body) => result

## Arguments and Values:

var := symbol

tree := tree structured list

return := form

body := declaration\* {tag | statement}\*

declaration := a declare expression; not evaluated.

tag := a go tag; not evaluated.

statement := a compound form; evaluated as described below.

result := if a return or return-from form is executed, the values passed from that form, otherwise the values returned by the RETURN or NIL if there is no RETURN form.

## Description:
Like CL:DOLIST, DOTREE iterates over the leaf (i.e. non-NIL atom) of a TREE.
The body of DOTREE is like a TAGBODY.
It consists of a series of tags and statements.

DOTREE evaluates TREE, which should produce a tree structured list.
It then executes the body once for each leaf of the TREE, in the order in which the tags and statements occur, with VAR bound to the leaf.
Then RETURN form is evaluated.
Tags label statements.

An implicit block named NIL surrounds DOTREE.
CL:RETURN may be used to terminate the loop immediately without performing any further iterations, returning zero or more values.

The scope of the binding of var does not include the TREE form, but the RETURN form is included.

Unlike CL:DOLIST, DOTREE accepts dotted list as TREE.

## Example:

## Affected-By:

## Side-Effects:

## Notes:
DOTREE is equivarrant `(dolist(l (alexandria:flatten tree)) ...)`, but faster than it and less consing than it. (especially in sbcl, but clisp.)

## Exceptional-Situations:

## See-Also:

TREE
