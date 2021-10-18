# TRESTRUL 0.0.6

## What is this?
Tiny utilities for TREe-STRUctured-List.

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

### Product's goal
merged by other utility libraries such as alexandria
### License
Public Domain

### Tested with
* SBCL/2.1.7
* CCL/1.12.1
* CLISP/2.49
* ECL/21.2.1
* Allegro/10.1
* CMUCL/21D
* ABCL/1.8.0
