Advanced Compiler Construction (CS 491): Exercise 3
===================================================

Tristan & Andrew

Running it
----------
To get the executable run `cabal install` while in the directory, and that should put an executable in your `~/.cabal/bin` directory. Otherwise use `cabal repl` and use the function `ghci> parseFile "filepath"`. The command `cabal run test` will run many of the tests. 

Assignment Additions
--------------------
In this assignment, we added reduction semantics, the CC machine, de Bruijn notation, and recursive types.

Completion Matrix
-----------------
| | Parsing | Typing | CBV | DB | RedSem | CC | 
|-|---------|--------|-----|----------|---------|----|
| Abs, App, Var    |✅|✅|✅|✅|✅|✅|
| Const & PrimOp   |✅|✅|✅|✅|✅|✅|
| Bool             |✅|✅|✅|✅|✅|✅|
| Char             |✅|✅|✅|✅|✅|✅|
| Unit             |✅|✅|✅|✅|✅|✅|
| Record           |✅|✅|✅|✅|✅|✅|
| Variant          |✅|✅|✅|✅|✅|✅|
| Rec Type         |✅|✅|✅|🟥|🟥|🟥|
| Mut Rec types    |✅|✅|✅|🟥|🟥|🟥|
| If               |✅|✅|✅|✅|✅|✅|
| Fix              |✅|✅|✅|✅|✅|✅|
| Let binding      |✅|✅|✅|✅|✅|✅|


Reduction Semantics
-------------------
The reduction semantics first required making the EvaluationContext, which follows from the in class examples. We instanced it into `Show` for debugging purposes, but I also wanted to use more unicode. The Reduction Semantics evaluator has three actions:

- `makeEvalContext`: this finds the next step to take
- `makeContractum`: 
- `fillWithTerm`: 


Sections
--------
Tristan: Reduction Semantics, DeBruijn, CCMachine  
Andrew: ...