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
|-|---------|--------|-----|----|--------|----|
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
| Let              |✅|✅|✅|✅|✅|✅|


Reduction Semantics
-------------------
The reduction semantics first required making the EvaluationContext, which follows from the in class examples. We instanced it into `Show` for debugging purposes, but I also wanted to use more unicode. The Reduction Semantics evaluator has three actions:

- `makeEvalContext`: this finds the next action to take in the term given, and remembers where that term goes in a context.
- `makeContractum`: this acts on a term and takes a small step on it.
- `fillWithTerm`: this puts a term back into a context where ther hole is.

Looping over these three steps until we have a value is equivalent to the small step semantics we have defined. It really just splits of the searching and evaluating sections.  

The `show` function proved very helpful for some debugging
```haskell
instance Show Context where
    show e = case e of
        Hole         -> "□"
        AppT e t2    -> "app(" ++ show e ++ ", " 
                            ++ show t2 ++ ")"
        ...
```
Everything else was very straight forward.

CC Machine
----------
This uses the Evauation Contexts and `makeEvalContext` from Reduction Semantics. The CC machine repeatedly makes `ccMachineStep` calls which each takes an action to get us closer to the solution. This works by continuously making small step changes and putting where that item goes into a context until it reaches a point where it has some value and a context. Then it finds the next point in that context to take a small step on. We implement this by reusing the `makeEvalContext` funciton, since it finds the next location that needs to be evaluated. 

The CC machine has a state that is 
```haskell
data MachineState = (Term, Context)
```
and the function that runs it is:
```haskell
ccMachineLoop :: MachineState -> Maybe MachineState
```
We avoided rewriting the logic in `shift` by using the `makeEvalContext` function
```haskell
shift :: S.Term -> E.Context -> Maybe MachineState
shift _ E.Hole = Nothing
shift v' e'    = RS.makeEvalContext $ e' `E.fillWithTerm` v'
```


DeBruijn
--------
Implementing DeBruijn CBV requires a new set of Term data types, a new `|->` implementation, and shift function now that we do not have named variables. Instead of writing a new parser we use the same parser and add a function that converts from a regular term to a DeBruijn term. 

The most novel part of DeBruijn is the `shift function
```haskell
-- | See TAPL pg. 79 for details
shift :: Int -> Int -> Term -> Term
shift c d t = case t of
    Var k       -> Var (if k < c then k else k + d)
    Abs tau t1  -> Abs tau (shift (c+1) d t1)
    App t1 t2   -> App (shift c d t1) (shift c d t2)
    ...
```
When writing the evalutator for the DeBruijn terms it was almost identical to the standard CBV. Instead of substituting a variable or grabbing a value with a label we instead shift by a number and index into things:
```haskell
S.Project t1@(S.Record labelsAndTerms) l1
    | isValue t1 -> lookupOrElse l1 labelsAndTerms "not in record"
-- becomes
S.Project t1@(S.Record ts) i1 _
    | isValue t1 -> return $ ts!!i1

```
When performing applications we have to be careful. We need to shift indecies inside `v2` up, then replace all isntances of zero with the shifted v2, and finally shift all the values back down afterwards. 
```haskell
-- pg 103: E-AppAbs
S.App (S.Abs _ t12) v2
    | S.isValue v2 -> return 
        (S.shift 0 (-1) ((0 |-> S.shift 0 1 v2) t12))

{- OR LIKE THE OTHER TEAM -}

apply :: S.Term -> S.Term -> S.Term
apply tBody v2 = S.shift 0 (-1) ((0 |-> S.shift 0 1 v2) tBody)
-- then just use apply
```

I also added a unicode alias to `shift` but its not used right now
```haskell
(↑) = shift
```

Sections
--------
Tristan: Reduction Semantics, DeBruijn, CCMachine  
Andrew: ...