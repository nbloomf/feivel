Overview
--------

The code of feivel is organized into five layers:

* The library layer, where the actual data structures and computations are defined,
* The Grammar layer, where the AST of the feivel language is defined,
* The Parsing layer, where (surprise!) the parser for the feivel language is defined,
* The Evaluation layer, where the code for reducing ASTs is defined, and
* The Interface layer, where the GUI and CLI are defined.

We can visualize these layers - literally - by calling "make vis" in the project root. This uses graphmod to build an SVG representation of the tree of modules in the Feivel project.

We use custom monads, ParseM and EvalM, for managing two types of stateful computations (parsing and evaluation). All IO, randomness, state, etc. (except at the top level, in main) is done via interfaces to these monads.

Like many Haskell projects, the organization of Feivel can be understood in terms of a handful of type signatures.

* Expr is the type of Feivel expressions. The complete grammar involves several different kinds of typed subexpressions, as well as explicit type annotations and source location information on the AST (for error reporting). The specific definition of the expression grammar changes from version to version as features are added. But the two most important functions involving Expr, get and put, don't change. These have the following signatures.

    put :: Locus -> a -> Expr
    get :: Expr -> Either GetErr a

put takes a value of type a and a "location" (obtained during parsing) and wraps them into an Expr. get takes an Expr and attempts to extract a value of type a; if it fails it instead returns a GetErr. These two functions are essentially used to take functions on pure values (Integers, Rationals, Polynomials, etc.) and lift them to functions on Exprs: we use get to extract parameter values and then apply put to the result.

* The EvalM monad allows us to safely express and compose stateful "evaluations". The most important function in this monad is eval, with the following signature.

    eval :: a -> EvalM a

In practice, eval takes an Expr and simplifies it. This computation takes place inside the EvalM monad because it may involve performing IO, changing state, or using randomness; all of this badness is hidden away in EvalM.


Adding new features
-------------------

Adding new features to Feivel is (mostly) straightforward. Exactly how this is done depends on the kind of feature.

* Adding a new function to an existing data type. This requires (0) implementing a pure function which does the computation, (1) amending the grammar, (2) amending the parser, and (3) amending the evaluator.

* Adding a new data type. This requires (0) implementing an associated pure structure to represent the data type, (1) writing a grammar, (2) writing a parser, (3) writing an evaluator.
