We are very happy to announce the release 0.2.0 of `gospel`.

Gospel is a tool-agnostic behavioural specification language for OCaml.
It allows to write strongly typed contracted based specifications to your
libraries (for a reasonnable subset of OCaml). Its syntax has been designed
in order to be easy to learn for an OCaml programmer.

This release adds two main features, a `gospel dumpast` command and a
`gospel.ppx` ppx rewriter to display gospel contents as documentation with
odoc.

Some minor extensions have been added to the language itself:
- a `with` construct to name a variable in type invariants referring to a
  value of the specified type,
- `int` literals,
- anonymous functions now support both patterns in arguments and return type
  annotations,
- unit result in function header,
- constants can now be referenced in specifications,
- infix operators are now accepted in specification headers.

Parser, preprocessor and error messages have been improved. In particular the
preprocessor can now handle large files and locations are properly tracked.

Pattern matches are now checked for exhaustiveness and redundancy.

A number of improvements and bugfixes in the type checker.

Some minor modifications in the Gospel standard library.

Documentation has been revised.

