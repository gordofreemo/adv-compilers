Exercise 4
==========

Andrew: CEK Machine, ...
Tristan: CK Machine, Natural Semantics

Running it
----------
If you compile the code (in `app\main.hs`) and pass the filename for the corelambda file. You can run the same function in ghci with `mainCompiler`. The output will look like:
```bash
ghci> mainCompiler "corelambda_files/sumn100.corelambda" 
app(fix(abs(f: ->(Int,Int). abs(x: Int. 
if IntEq(0, x) then 0 
else IntAdd(x, app(f, IntSub(x, 1))) fi))), 100)
Free Variables:[]
Type:Int
Call-by-value evaluation: 
        5050
Natural CBV: 
        5050
Natural CBV Closures: 
        5050
Natural CBV Closures & DB: 
        5050
BeBruijn evaluation: 
        5050
Standard Reduction evaluation: 
        5050
CC Machine evaluation: 
        5050
CK Machine evaluation: 
        5050
CEK Machine evaluation: 
        "not working :("
```

Natural Semantics
-----------------
Natural (or Big-Step) semantics takes larger steps than the small step rules we have been using, [defined here](https://www.cs.tufts.edu/~nr/cs257/archive/gilles-kahn/natural-semantics.pdf). The general constraint here is that everything should be done with one pattern match. For example:

```haskell
-- in small step cbv we would do:
...
S.If (S.Const S.Tru) t2 _   ->  ...
S.If (S.Const S.Fls) _ t3   ->  ...
S.If t1 t2 t3               ->  ...

-- while in big step we do:
S.If t1 t2 t3 -> do
                    S.Const b <- eval t1
                    if b == S.Tru
                      then eval t2
                      else eval t3
```
Having this constraint leads to natural semantics. There is really nothing else special here.


Natural Semantics with Environments and Closures
------------------------------------------------
We introduce a few new things:

1. Environment: An environment is a map of variables to values. Instead of substituting (x |-> t) we hold all the 
2. Closures: Closures are a term that needs to be evaluated in a certain environment.

Natural Semantics with Env, Clo, and DB Indices
-----------------------------------------------
...

CK Machine
----------
...

CEK Machine
-----------
...
