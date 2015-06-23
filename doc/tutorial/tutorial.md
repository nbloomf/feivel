% a `feivel` tutorial

This tutorial is Copyright 2015 Nathan Bloomfield and licensed under the [Creative Commons BY-SA license version 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/).

Introduction
============

The Elevator Pitch
------------------

Feivel is an extremely simple template processing language. The primary audience is math teachers who are writing problem sets. For example, the input may be a "generic" problem set on factoring polynomials with the coefficients unspecified, and the output is a specific problem set with numbers chosen for the coefficients (either randomly, or from a store of data, or as the result of some calculation). In this way, once the generic problem set is written we can easily generate as many different-but-similar copies as we want. In practice, the output will typically be in e.g. LaTeX, but `feivel` operates on streams of plain text and so might be applied to other uses as well.

`feivel` is free software and licensed under Version 3 of the GNU GPL. For installation instructions, see the `README`.


First Steps
-----------

The user interface of `feivel` is command-line only. You will need to be comfortable editing plain text and running basic commands from a shell prompt.

To use `feivel`, we prepare a specially formatted text file called a *template*, and then *process* this template from the command line.

For example, here is a very simple template. (We will use the convention that lines starting with `>>>` delineate the contents of a text file.) Try typing or pasting this text into a file called `ex01.fvl`.

    >>> ex01.fvl
    [define int @k := Uniform(1;6)]#
    You rolled a @k!
    
    >>>

The stuff inside the [square brackets] is a *command*. This particular command defines an integer, called `@k`, to be the value obtained by observing a uniform random variable on the set {1,2,3,4,5,6}. `@k` is a *key*, which now has an associated *value*. When this key is used outside square brackets, as on the second line, it is replaced by its value. We can process this template by invoking `feivel` at the command line like this.

    > feivel -t ex01.fvl

This command processes the file `ex01.fvl` as a (`-t`) template. When this line is executed, we should see output like

    > You rolled a 5!

Try processing this template several times; you should see different numbers. There are several commands and expression types available; here is another example.

    >>> ex002.fvl
    [define [int] @a := [[1;2];[3;4]]]#
    [define [int] @b := [[1;4;7];[-1;2;0]]]#
    [:str: Tab(int; @a * @b) :]
    
    >>>

If we process this template using 

    > feivel -t ex02.fvl

we should see something like

    > [ -1  8  7 ]
    > [ -1 20 21 ]

This time we defined two matrices, computed their product, and displayed the result in tabular form. In this tutorial, we will introduce some of the different commands available in `feivel` and illustrate their use. For the impatient some larger examples, with makefiles needed to process them, are available in `doc/examples`.



Purpose
-------

Like other templating languages, `feivel` allows us to write documents which carry state, within which we can perform basic computations which affect the document itself. Why make a new language? Most existing templating languages are designed for the web. `feivel` is different: it is designed for math teachers writing problems. Where a more conventional templating language may be used to build a nicely formatted web page using data from a database (say), `feivel` can be used to build a nicely formatted math problem set using data chosen randomly (or maybe from a database if you want, but this feature is incomplete).

An important design goal for the `feivel` language is to be simple and unsurprising. The ability to write generic math problems is useless if the syntax needed to do so is too opaque or cumbersome.



Running `feivel`
-----------------

There are two ways to use `feivel`: as a unix-style *filter* (for producing documents) and as an interactive *repl environment* (as a calculator).

To use `feivel` as a filter, we can simply put it in a pipeline or call it with the `-t` flag set. The examples from the introduction use filter mode.

To use `feivel` as an interactive environment, run it with the `--repl` flag set. For example, try calling the following command.

    > feivel --repl

Then type this:

    > [:int: 2^30 :]

and you should then see this:

    > 1073741824

This is a read-evaluate-print loop, wherein we can essentially process a template one line at a time. This is an effective way to try out bits of syntax on the fly. To exit the REPL, say `[quit]`.

(N.B.: The REPL is extremely simple. At the moment (0.1.0) lines cannot even be edited, there is no command history, etc.. I have some ideas about making it much more useful in a future version.)



How it Works: The Basics
------------------------

In this section we will get a bird's eye view of the template language.

A template consists of **text** interspersed with **keys**, **commands**, and **expressions**. Processing a template happens in two steps: the **parsing** step, during which the template is parsed into an abstract syntax tree, and the **evaluation** step, during which this tree is evaluated to give a concrete output document.

During the evaluation step `feivel` keeps track of a **store** of key-value pairs which may be referred to in the template. The store may be populated with data at invocation time (before parsing the template), and certain commands can add new key-value pairs to the store. But once a key has been defined it cannot be undefined; the store is *write-once*.

`feivel` subscribes to the "fail early, often, and loudly" philosophy, and as such is extremely picky about which templates it will accept. Any errors either in syntax or during evaluation will result in *no output* (with a hopefully useful error message) rather than *incorrect output*. This is because it is assumed that `feivel` will be part of a larger pipeline processing data into documents, likely without human intervention, and in applications where it is preferable to force the template author to write more carefully than to distribute documents with garbage. This can be frustrating while debugging a template, but does inspire some confidence that a working template actually does something close to what you want.

**Text** is any string of characters not including `[`, `]`, `#`, or `@`. (These characters have a special meaning which will be explained shortly.) To include these characters verbatim in your template simply escape them with #. For instance, try typing `##`, `#@`, `#[`, and `#]` in the REPL. Text evaluates to itself.

**Keys** are strings of alphanumeric characters prefixed with `@`. Keys are evaluated by looking up their *value* in the store. Attempting to evaluate a key which has not been defined, or to redefine a key which has already been defined, is an error. For example, try typing the following into a fresh REPL, one line at a time.

    > @a
    > [define int @a := 2]
    > @a
    > [define int @a := 3]

**Commands** give us control over how a template is processed and are denoted by wrapping them in [square brackets]. One of the most basic commands is `shuffle`; this command takes a list of templates in [square brackets], evaluates them, randomly shuffles them, and then concatenates them. For example, try evaluating the following in the repl.

    > [shuffle [a] [b] [c] [d] [e] endshuffle]

You should see some permutation of the letters a, b, c, d, and e, with no spaces between. Repeating this command twice will (generally) yield different permutations each time. Note that the `endshuffle` keyword is optional; we could have written this command as

    > [shuffle [a] [b] [c] [d] [e]]

Every command has an optional `end` keyword. Also note that when feivel is interpreting text in "text mode", all spaces and newlines are kept verbatim, but in "command mode" spaces and newlines are irrelevant. For instance, the previous command could also have been written as follows (now we are using filter mode).

    >>> ex003.txt
    [shuffle
     [a]
     [b]
     [c]
     [d]
     [e]
    endshuffle]
    
    >>>

This gives us some ability to block-structure our templates for better readability.

**Expressions** are different from ordinary text and commands; they represent structured data upon which we can perform basic computations. This is where the bulk of the complexity of `feivel` lives. Every expression has a *type*, which determines how it is interpreted and what we can do with it. One basic type is `bool`, representing boolean values. The boolean constants true and false are denoted `#t` and `#f`, respectively. Try typing the following into the repl.

    > [:bool: #t :]

The repl should print `#t` back. Here we have used another command: [:square colons:] are used to denote a *naked expression* in the template; naked expressions are evaluated in place and replaced by a textual representation of their value. Here `#t` is the textual representation of the boolean true value. Naked expressions should be supplied with a *type signature*; this is the `bool:` here. Type signatures act as documentation for the template author and are required for the parser. Lots of expressions can be unambiguously parsed without a signature, but there is no fancy type inference going on and ambiguous expressions may cause `feivel` to bail. (N.B.: I would like to replace this with actual type inference, but hand-holding the parser will do for now.)

Types in `feivel` come in two flavors:

1. *atomic* types, which represent concrete data. As of 0.1.0 there are 4 atomic types: `bool` (booleans), `str` (strings of text), `int` (arbitrary precision integers), and `rat` (arbitrary precision rationals).
2. *constructor* types, which take a concrete type and construct a new concrete type. As of 0.1.0 there are 4 type constructors: `{typ}` (lists of `typ`), `[typ]` (matrices of `typ`), `^typ` (polynomials over `typ`), and `>typ` (macros of `typ`).

Constructors can be nested, so that (for instance) `[{int}]` represents the set of matrices whose entries are lists of integers. Macros are the strangest of these; they are like functions (but not quite) and allow us to wrap a complicated expression with parameters into a single value. An expression of type `>int` can be *evaluated* to *yield* a value of type `int`. Macros can also be nested, as we'll see later, so for instance an expression of type `>>int` is a macro that yields a macro that yields an integer, and an expression of type `[>rat]` is a matrix of macros that yield rationals. In that sense macros are "first-class" values. Why? Why not? :)



Commands
========

Here we will discuss the available commands. In the next two sections, `TEMP` will represent an arbitrary template; `EXPR` an arbitrary expression; `TYP` a type; `INT`, `RAT`, `STR`, `BOOL`, `LIST`, `MAT` an arbitrary expression of type integer, rational, string, boolean, list (of something), and matrix (of something), respectively; and `KEY` a key. These are syntactic variables; all other text in `monospaced font` is literal syntax.

`define`/`let`
--------------

Usage: `[define TYP KEY := EXPR enddefine]`

Usage: `[let TYP KEY := EXPR in [TEMP] endlet]`

`define` and `let` are both used to bind a value to a new key in the store; the only difference is the scope of the resulting binding. The scope of `define` is the remainder of the current template file, while the scope of `let` is the subtemplate TEMP.


`scope`
-------

Usage: `[scope [TEMP] endscope]`

`scope` allows us to limit the scope of store changes.


`if`/`cond`
-----------

Usage: `[if BOOL then [TEMP] else [TEMP] endif]`

Usage: `[cond case (BOOL) [TEMP] ... default [TEMP] endcond]`

`if` and `cond` are both used to switch on the value of a boolean expression called a *guard*.

`cond` evaluates each case guard in order until one is true, and then evaluates the corresponding subtemplate, ignoring the remaining cases. It has an optional default value which is only evaluated if all cases are false.


`shuffle`
---------

Usage: `[shuffle [TEMP] [TEMP] ... endshuffle]`

`shuffle` "randomly" permutes the templates provided. Do not depend on this randomness for anything important.


`alt`
-----

Usage: `[alt opt [TEMP] opt [TEMP] ... endalt]`

`alt` chooses a subtemplate at "random". Do not depend on this randomness for anything important.


`for`
-----

Usage: `[for TYP KEY in LIST say [TEMP] endfor]`

`for` binds KEY to the entries of LIST one at a time, evaluates TEMP, and concatenates the results. It is kind of like a restricted `concatMap`, if that means anything.


`select`
--------

Usage: `[select TYP KEY from LIST in [TEMP] endselect]`

`select` chooses a "random" element of LIST to bind to KEY in TEMP. Do not depend on this ramdomness for anything important.


`eval`
------

Usage: `[eval MAC (TYP KEY := EXPR; ...) endeval]`

`eval` is used to evaluate template macros.



Expressions
===========

(N.B.: Watch out - this part of the language is changing rapidly, so this may be incorrect or incomplete.)

The expression syntax of `feivel` is... kind of ad hoc, and may change. Expressions are either prefix, like `NAME(ARG1; ARG2; ...; ARGn)` or infix, like `ARG1 op ARG2`. Prefix function names are capitalized while infix function names are not. The exception is type casting functions, which are prefix and not capitalized.


General
-------

These expressions can be used over any type.

- `if BOOL then EXPR1 else EXPR2`

    Evaluates `BOOL`, then returns `EXPR1` if the result is "true" and `EXPR2` otherwise. Note that `EXPR1` and `EXPR2` must have the same type.

- `AtPos(LIST; INT)`

    Extracts the element at position `INT` from list `LIST`. Lists are indexed from 1. If `INT` is negative or larger than the length of `LIST`, then `feivel` bails.

- `AtIdx(MAT; INT1; INT2)`

    Extracts the entry at row `INT1` and column `INT2` from matrix `MAT`. Rows and columns are indexed from 1. If either of `INT1` or `INT2` is negative or greater than the number of rows/columns (respectively) of `MAT`, then `feivel` bails.

- `Rand(LIST)`

    Choose an element of list `LIST` at "random". As usual, do not depend on this randomness for anything important. If `LIST` is empty, then `feivel` bails.

- `Eval(TYP; MACRO; TYP KEY := EXPR; ...)`

    Evaluate a macro, returning a value of type `TYP`, using the provided context to overwrite the default values of the macro.


`bool`: Booleans
----------------

- `#t`, `#f`

    Boolean constants (true and false).

- `&&`, `||`

    Infix operators for logical "and" and "or".

- `Equal(TYP; EXPR1; EXPR2)`

    Compares `EXPR1` and `EXPR2`, which have type `TYP`, for equality. (Yes this is clunky. But it's easy to parse.)

- `LT(TYP; EXPR1; EXPR2)`

    Returns `#t` if `EXPR1` is less than `EXPR2`, where both expressions have type `TYP`. This only makes sense for ordered types (`int`, `rat`, and lexicographic `str` at the moment.) Similarly, we have `GT`, `LEq`, and `GEq`.

- `IsDefined(KEY)`

    Returns `#t` if `KEY` is defined and `#f` otherwise.

- `Divides(INT; INT)`

    Infix integer "divides" relation; e.g. `Divides(2; 4)` evaluates to `#t`.

- `Matches(STR; PAT)`

    Infix regular expression matching relation. `PAT` is a string constant, interpreted as a regular expression. Returns `#t` if `STR` matches the pattern and `#f` otherwise.

- `Elem(TYP; EXPR; LIST)`

    Returns `#t` if `EXPR` is an element of `LIST` and `#f` otherwise, using the same equality test that `Equal` uses. `EXPR` should have type `TYP` and `LIST` should have type `{TYP}`.


`int`: Integers
---------------

- Integer constants are specified in decimal with an optional negative sign; e.g. `27` or `-31`.

- `+`, `-`, `*`, `^`

    Infix arithmetic as usual. Trying to evaluate a power with a negative exponent is an error.

- `min`, `max`, `gcd`, `lcm`, `choose`

    Infix operations.


`str`: Strings
--------------

- String constants are denoted using "double quotes"; to include a double quote in your string, escape it with a backslash. E.g. `"hello"` and `"hel\"lo"`.

- `++`

    Concatenate.

- `Reverse(STR)`

    Reverse a string.

- `Decimal(RAT; INT)`

    The decimal form (in ascii) of RAT with INT digits after the radix point.

- `Rot13(STR)`

    Maps "a" to "m", "b" to "n", and so on.

- `Roman(INT)`

    Prints INT in Roman numerals. Bails if given a number less than 1 or larger than 4000.

- `Tab(TYP; MAT)`

    Pretty-print (in ascii) a matrix with entries of type TYP. (N.B.: The exact functionality of this command will change in the future.)


`rat`: Rationals
----------------

- Rational constants are written inline as `a/b`, where `a` and `b` are integer constants.

- `Sqrt(RAT; INT)`

    Approximate the square root of RAT as a rational number with at least INT correct decimal digits.

- `int(INT)`

    Cast `INT` to a rational number; e.g. express n as n/1.


`{t}`: Lists
------------

Lists consist of 0 or more expressions, all of the same type, in a fixed order.

- List constants are wrapped in {curly braces}, with items separated by semicolons. E.g. `{1;2;3+5}` is a list of integers.

- `++`

    Infix concatenation.

- `Shuffle(LIST)`

    "Randomly" permute the entries of `LIST`. As usual, do not depend on this randomness for anything important.

- `Choose(INT; LIST)`

    Choose a "random" sample of `INT` entries of `LIST`. In general the entries in the sample will not be in the same order they were in in `LIST`. As usual, do not depend on this randomness for anything important.

- `Reverse(LIST)`

    Reverse the order of the elements of `LIST`.

- `Choices(INT; LIST)`

    Return a list of all possible `INT`-element index-wise samples from `LIST`. The samples will be in the same order they were in in `LIST`. (By index-wise, I mean that `Choices(2;{1;1;1})` will return `{{1;1};{1;1};{1;1}}`, as each of these `1`s has a different index in `{1;1;1}`.)

- `Filter(KEY; BOOL; LIST)`

    Take each entry of `LIST` and bind it to `KEY` before evaluating `BOOL`. Return a list of only those elements of `LIST` (in order) such that `BOOL` evaluates to `#t`.

- `Build(EXPR; BIND/BOOL; ...)`

    List builder notation. Constructs a list of all expressions of the form `EXPR`, where `EXPR` may contain keys which are bound inside `BIND`, and where each `BOOL` expression must evaluate to `#t`. An example is worth a thousand words: `Build(@a ++ @b; str @a <- {"a";"b"}; str @b <- {"cd";"ccd";"d"}; Matches(@b; "c.d"))` returns `{accd;bccd}`

- `GetRow(INT; MAT)`, `GetCol(INT; MAT)`

    Extract a row or column from the given matrix. INT is the index (counting from 1) of the desired row/column.


`[t]`: Matrices
---------------

A matrix is a rectangular array. Arrays of numeric types have a richer arithmetic, but we can still deal with matrices of e.g. strings.

- Matrix constants are denoted row-wise, with rows (and the whole matrix) wrapped in [square brackets] and entries separated by semicolons. E.g., `[[1;2];[3;4]]` is a matrix of integers.

- `+`, `*`

    Arithmetic as usual for matrices of integers, rationals, or booleans.

- `hcat`, `vcat`

    Infix matrix concatenation

- `GetRow(INT; MAT)`, `GetCol(INT; MAT)`

    Extract the row or column matrix at index INT (counting from 1).


`^t`: Polynomials
-----------------

A polynomial is a partial map over a set of string-like expressions called monomials. Maps over numeric types have a richer arithmetic, but we can still deal with polynomials over e.g. strings. (Why? Why not?) To make parsing simpler, the syntax for defining polynomials is a little awkward. For example, the polynomial x^2 + 2x + 1 is denoted `Poly(1.x^2; 2.x; 1.1)`.

(N.B.: I will defer this part of the documentation for now as it will probably be out of date soon. Consider polynomials to be experimental for now.)


`>t`: Macros
------------

A macro is a typed subtemplate. Any keys inside a macro must be given default values at definition time; these can be overwritten at evaluation time. Macros remember the store at definition time and so may refer to "ambient" keys.

Macro constants are denoted by `Macro(TYP; EXPR; TYP KEY := EXPR; ...)`. The first `TYP` parameter is the "return type" of the macro, the type of its result. The middle `EXPR` parameter is the *body* of the macro, which is evaluated when the macro is called. The remaining arguments are the *default parameters* of the macro.

To evaluate a macro, we say `EVAL(TYP; MAC; TYP KEY := EXPR; ...)`. The first `TYP` parameter is the return type of the evalation. The `MAC` parameter is the macro being evaluated. The remaining arguments are the parameters of the macro. When a macro is evaluated with `Eval`, any parameters supplied here will overwrite their default values.

For example, try typing the following into a fresh REPL.

    > [define >int @m := Macro(int; @i + 2; int @i := 5)]

This defines a macro which takes an integer and adds two to it. By default the integer added is 5 which we can see by evaluating this macro with no parameters.

    > [:int: Eval(int; @m) :]

By the way, we can get the same result by simply saying `@m` in text mode.

    > @m

We can change the value of `@i` in the body of this macro by supplying a parameter as follows.

    > [:int: Eval(int; @m; int @i := 11) :]

Macros can have any number of parameters, any number of which can be redefined (in any order) when we use `Eval`.

Macros are kind of like very clunky subroutines where all parameters are named and have default values. At the moment recursion is not possible -- a macro cannot be directly evaluated inside its own body. (N.B.: Recursion is a possible future feature. In the meantime there may be a way to approximate it using a helper function with a default function parameter that gets called with itself, but I haven't tried it.)

One last thing: it is important to note that macro bodies are evaluated afresh at *invocation time*, that is, at every call to `Eval`. For example, define the following in a fresh REPL.

    > [define >int @f := Macro(int; Uniform(1;10))]

Now evaluate this macro with several `@f`s.

    > @f @f @f @f

You should see (in general) different numbers. This is because every time `@f` is evaluated, its body is evaluated, meaning we observe a different number from `Uniform` each time. Contrast this with the following similar command.

    > [define int @i := Uniform(1;10)]
    > @i @i @i @i

This time we see the same number repeated. This is because `@i` was only evaluated *once*, at *define time*. Of course we can get a similar effect, fixing the value of `@f` using `Eval` like so.

    > [define int @j := Eval(int; @f)]
    > @j @j @j @j

The difference between evaluating at definition time and invocation time may allow some interesting uses, such as the "die roll" example here -- `@f` is effectively a d10.
