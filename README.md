# TRESTRUL 0.0.0 - Tiny utilities for TREe-STRUctured-List.

## Current lisp world
List is very flexible.
It can be represented as tree too.

## Issues
Although CL lacks operators for tree structured list.

IMO: Reason of lackness may it is tough to get consensus about tree operations.
In lisp, list is constructed by CONS.
Consider removing 1 from '(1 . 2).
Additionally, if it is tree, we can represents nested list as node.
If so '(nil) contains one node? or two node?

## Proposal
TRESTRUL provides it.

## Usage

## From developer

* Product's goal - merged by other utility libraries such as alexandria
* License - public domain
* Developped with - CLISP
* Tested with - SBCL CCL ECL

