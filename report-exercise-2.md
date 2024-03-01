Advanced Compiler Construction (CS 491): Exercise 2
===================================================

Tristan & Andrew

Running it
----------
To get the executable run `cabal install` while in the directory, and that should put an executable in your `~/.cabal/bin` directory. Otherwise use `cabal repl` and use the function `ghci> parseFile "filepath"`.


Assignment Additions
--------------------
There were a few additions to the core language. We added `Unit` and `Char` along with a few functions and a test, but these were basically trivial as they did not add anything to `eval1`. 

### Records
`Record`s are product types and have multiple named elements of specified types. The constructor for `Record`s is
$$
\text{record} (l_1=t_1, l_2=t_2, ...,  l_n=t_n) : \text{Record} (l_1=t_1, l_2=t_2, ...,  l_n=t_n)
$$
and the eliminator is
$$
\text{project} (\text{record} (l_1=t_1, l_2=t_2, ...,  l_n=t_n).l_j) 
\rightarrow t_j
$$
We used the small step semantics from TAPL. The type checking here was simple, as the internal structure of the values were:
```haskell
data Type =  ...
          |  TypeRecord     [(Label, Type)]
data Term =  ...
          |  Record      [(Label, Term)]
          |  Project     Term Label
```
These let us use `lookup` to easily find the type a term should be, and the term that project needs from label. Since `lookup` returns a `Maybe` we just wrote a function `lookupOrElse` which returned an `Either` or our error system. 

### Variants
`Variant`s are sum types and have one of multiple possible named elements of specified types. The constructor for a `Variant` is
$$
\text{tag} (l=t \text{ as } \text{Variant} (l_1:T_1, l_2:T_2, ...,  l_n:T_n)) 
: \text{Variant} (l_1:T_1, l_2:T_2, ...,  l_n:T_n)
$$
and the eliminator is
$$
\begin{aligned}
    &\text{case } t \\
    &\text{of } l_1 = x_1 \Rightarrow t_1 \\
    &\text{| }  ... \\
    &\text{| }  l_n = x_n \Rightarrow t_n \\
    &\text{esac} 
\end{aligned}
$$

The implementation of `Variant` was significantly more work than `Record`. 
```haskell
data Type =  ...
          |  TypeVariant    [(Label, Type)]
data Term =  ...
          |  Tag         Label Term Type
          |  Case        Term [(Label, Var, Term)]
```
`Tag` was not too difficult, but `Case` proved to require more thoughtfulness since it requires that we do a lot in the type checking:

1. The labels in the case body and in the term being cased against need to match.
2. All terms in the case body need to evaluate to the same type
    - Additionally those terms need to be placed in a new context because of the variable that is bound to each one, but not all of those terms.  

Here is the code without the `where` clause:
```Haskell
{- lib/Typing.hs -}
S.Case t1 lvt -> case typing gamma t1 of   -- match Case term
        Right tau1@(S.TypeVariant lts) -> do -- t1 is a Variant
                 -- same labels
                if sort labelsBody == sort labelsFocus
                        then isSameType
                        else Left "errMsg"
            where ...
```

After the type checking everything else was easy, as it was also just right out of TAPL. 

Variants and records have all type checking incorporated into our type error framework. 

### Let
...

### Fix
...

Introducing Testing
-------------------
We are using the cabal testing framework. To run the test suite use `$ cabal run test` in the cabal directory. 
We are using the `HUnit` module to make the tests. 
We have it set up so we can test a variety of aspects of a file:

- Does the file parse?
- Does the file have a certain list of free variables?
- Does the file type check?
- Does the file evaluate to something? (i.e. making sure it does not diverge or runtime error)
- Does the file evaluate to a specified value?

We encompass these in a list of the form:
```Haskell
-- test/Test.hs
data TestType = NoParseError
              | NoTypeError
              | SolutionIs String
              | FreeVars [String]
              | EvalsToSomething

testAnswers :: [(FilePath, TestType)]
testAnswers = ...
```