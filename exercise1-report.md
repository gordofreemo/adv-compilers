# Exercise 1 Report

## FV/SUBSTITUTION

Our definition of the free variable function was based on the definition provided in TAPL, with appropriate modifications to deal with If statements, Constants, and Primitive Operations. In the case of If statements and Primitive Operations, we simply take the union of free variables for all subterms, and constants are assumed to contain no free variables. Otherwise, a variable has free variables equal to itself, and the free variables of an abstraction consists of the free variables of the subterm minus the variable being abstracted. All this functionality is implemented using lists and the bing operator (>>=).

For substition, we base our implementation of the definition in the book. However, unlike the book, we did not implement variable renaming using alpha-equivalence in order to deal with cases of shadowing. We assume that our programs will not contain such issues. Otherwise, our definition of substition matches that of the book.

## Parser

For parsing, we have utilized the Parsec library. We first implement some primitives to parse identifiers, tokens, whitespace, comments, etc. After implementing some parsing primitives, implementing the BNF grammar provided is straightforward. In case of a parse error, we propagate the ereror upwards.

## Typechecker

We expanded the Typechecker in two aspects. Starting off, we expanded the type checker to properly deal with Primitive application by expanding the pattern matching cases. Furthermore, we replaced the Maybe type with the Either type in the type declaration, and added informative type error messages using it. 

## Semantics/Evaluator

Our only addition to the operational semantics of our lambda calculus has been implementing the semenatics of a primitive operation application, which involves evaluating all the subterms, evaluating the operation, and then applying the evaulated operator onto the evaluated subterms. 
To evaluate a primitive application, we updated the operational semantics to contain 

```haskell
  S.PrimApp op xs -> do xs' <- (sequence $ fmap eval1 xs); Just (S.primOpEval op xs')
```

## Main Function

Our main function runs each component implemented and displays information about it. We parse command line arguments, read the file named by then, and then runs the parser, Typechecker, and evaluator in order. For each component, we display the relevant output: entire program for the parser, type or type error for the Typechecker, and normal form of the program for the evaluator. At the moment, we do not stop evaluation in the case of a type error. This can be easily fixed, but for the moment it allows us to evaluate programs that do not terminate in a value. 
