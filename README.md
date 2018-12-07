Hestu Language
==============

![Korok](./docs/img/korok.png)
[Korok in a nutshell](http://zelda.wikia.com/wiki/Korok)

Final project for Compiler Construction course.

Get latest changes on the [GitHub page](https://github.com/ratijas/cc).

# Ya-ha-ha!

To build project haskell-stack toolchain is needed.

 1. Obtain stack here: [stack](https://docs.haskellstack.org/en/stable/README/).
 2. Run `$ stack build` under project directory
 3. Wait for stack to download an appropriate Haskell compiler and build the project with all its dependencies.
 4. Run `$ stack exec hestu` to run REPL or `$ stack exec hestu finename.yahaha` to run a script from a file.

Run all test scripts with `$ ./run-tests.sh`

# Implementation

 - Hestu is simple functional language.
 - Syntax is better described by Hestu parser sources and is subject to changes.
    * Expressions are in C-style
    * Case-sensitive
    * Operator precedence is fully supported:
        - function calls, indexing, member access and type checking (`is` operator);
        - unary plus, minus and `not`
        - multiplication and division
        - addition and subtraction
        - comparison: < <= > >= = /=
        - logicals: `and`, `or`, `xor`
    * 3 types of loops:
        - `for variable in lower .. upper loop [statements] end`
        - `while condition loop [statements] end`
        - `loop [statements] end`
    * if-then-else-end where "else" part is required
    * `return [expression]` statement supported in functions
    * Function definitions:
        - short style `func(arguments) => expression`
        - full style `func(arguments) is [statements] end`
    * Variable declaration with `var x [:= expression]` and assignment with `x := expression` respectively
    * All variables MUST be defined before being accessed / assigned

 - Hestu scripts use reserved file name extension `.yahaha`
 - If you happen to encounter "Yahaha" exception, it means either null pointer exception (usage of an uninitialized variable) or some internal VM incompleteness (functionality not implemented).
