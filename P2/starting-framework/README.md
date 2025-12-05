# B3TC Lab 2

This directory contains an un-finished video game.

The video game (un-realistically) simulates the a spaceship in a field of asteroids.

The spaceship is controlled by programs written in the _Arrow_ programming language, which you are tasked with implementing.

The Arrow language is described in `arrow-format.md`.

## Tasks

1. (10 cp). 
    Write a lexer/scanner for the language using Alex. Define a datatype to
    represent tokens and let the scanner return such tokens. Study the online documentation
    of Alex to find out what the syntax of Alex specification files is. Start in Chapter 2,
    the introduction should already contain most of what you need. The use of the basic
    wrapper is sufficient for this task. If you want to use one of the more advanced wrappers,
    that is also fine, but not required. In particular, terminating with an exception on a
    lexing error is allowed in this assignment. Remember to test your implementation!
    Write the datatype in `Model.hs`, and the lexer in `Lexer.x`.

2. (10 cp). 
    Define a suitable abstract syntax for the Arrow language in `Model.hs`.
    Call the type corresponding to a whole program `Program`.

3. (15 cp). 
    Write a parser for the language using Happy, in `Parser.y`. Again, study
    the online documentation to find out about the syntax of Happy specification files.
    Again, Chapter 2 is a good start and should contain most of the required information.
    Use the datatype of tokens delivered by the lexer as input, and produce values of the
    abstract syntax as results of the parser. It is not required to use any of the advanced
    error-handling functionality of Happy, nested lexing, or monadic parsing functionality.
    Again, failing with an exception on a parse error is allowed, and remember to test your
    implementation!

4. (25 cp). 
  Define an algebra type and a fold function for your abstract syntax type, in `Algebra.hs`.

5. (25 cp). 
  Define one or more algebras that describe an analysis of the program that
  (besides possibly required additional information) performs the following sanity checks
  on a given program:
    * There are no calls to undefined rules (rules may be used before they are defined though).
    * There is a rule named start.
    * No rule is defined twice.
    * There is no possibility for pattern match failure, i.e., all case expressions must 
      either contain a catch-all pattern _ or contain cases for all five other options.
  
  Use the algebras and fold functions to define a function
  `check` that combines all the above checks and returns true iff a program is sane.

6. (5 cp). 
  Write a function `toEnvironment` in `Interpreter.hs` that first lexes and then parses a string, 
  checks the resulting Program using check , and, if the check succeeds, 
  translates the Program into an environment.

7. (30 cp). 
  During the execution of a program, we have to maintain state. The state
  contains the current space, the position of Arrow, its heading, and a stack of commands.
  Implement a function in `Interpreter.hs` that performs a single execution `step`.
  The function implements the following semantics. The top item on the command stack
  is analyzed:
    * On go, Arrow moves forward one step using its current heading, as long as the
      target field is empty or contains a lambda or debris. Otherwise, it stays where it
      is.
    * On take, Arrow picks up lambda or debris, leaving an empty space at its current
      position.
    * On mark, Arrow places a lambda at its current position regardless of what was
      there before (debris is removed).
    * On nothing, nothing changes.
    * On turn, Arrow changes its heading by 90 degrees to the left or right as indicated.
      Turning forward is possible, but has no effect.
    * On a case, Arrow makes a sensor reading. Depending on the direction specified
      as an argument to case, Arrow will take a look at the position that (according to
      its current heading) is to the front, left, or right. The pattern of each alternative
      is then analyzed in turn until one matching alternative is found. The instructions
      on the right hand side are then prepended to the command stack and execution
      continues. If no alternative matches, execution fails. An alternative matches if the
      pattern corresponds to the contents. Positions that are not stored in the finite map
      are implicitly assumed to contain `Boundary`. A catch-all pattern matches always.
    * On a rule call, the code stored with that rule in the environment is prepended to
      the command stack. If the rule is not defined, execution fails.
    * If the command stack is empty, a `Done` result is produced.

8. (30 cp). 
  Write two drivers `interactive` and `batch` in Main.hs that (given an environment and 
  an initial state) run the program. The interactive driver should print in every step at 
  least the board and ask for user confirmation by having them press Enter. After getting the user input, 
  the driver should invoke the next step and continue from the beginning. This driver should 
  recognize abnormal and successful terminations of the reduction and treat them sensibly, by printing a clear 
  error message, starting with "error: ". The batch driver should run the program and return the final state, in
  which there are no more steps to take. Write a proper main function that lets you read in a space and a program 
  from a file, specify a start position and heading, and runs the program in either mode.
  Look at `driver-format.md` for the precise command-line formatting we require.

## Alex and Happy
This assignment uses the parser and lexer generators Alex and Happy, 
whose documentation is available here:
http://haskell.org/alex/
http://haskell.org/happy/

If you use an up-to-date Cabal, you should be able to simply code in the .x and .y files.
Running `cabal build` or `cabal run` will compile the alex and happy code into Haskell
files, and compile your project with those, without ever showing them to you.
To test if your setup works, `cabal build` on the starting project should succeed, and `cabal run` should give "lexical error".

`cabal repl` gives you a GHCI session, but `:r` might not recompile the Lexer and Parser:
To be safe, simply `:q` and re-enter it.

You can also manually call the alex and happy executables and then run ghc(i) 
on the resulting Haskell files, but then you have to remember to do so on each change, 
or you'll run the old version again: Not the recommended workflow!

If you're ever in doubt, delete any `Lexer.hs` and `Parser.hs` files.

## Testing

To test your lexer and parser, you can simply run `cabal run`, which executes `main` in `Main.hs`.
Try your parser and lexer on different inputs, the `examples` directory contains a few `.arrow` programs with corresponding `.space` layouts.

## Submission Instructions

* Create a zip archive containing the `src/` directory and its contents

* There will be multiple chances to hand-in your implementation. Make sure your version compiles before handing in,
  otherwise you will miss out on the chance to receive proper feedback.

* Do not change any of the type signatures present in the template,
  and only change the data type definitions that are currently empty
  (such as `Token` or `Ident`).

* Ensure you fully understand your code & can answer questions about it in an oral exam.

    You will find this easier if you write your code readably and in an idiomatic Haskell style, e.g.

    * Include useful comments in your code.
        Do not paraphrase the code, but describe the general structure, special cases, preconditions, invariants, etc.
    * Use existing higher-order functions (e.g. `map`, `foldr`, `filter`, `zip`).
    * Use existing libraries (see the cabal file for suggestions)

* Read through and be bound by the LICENSE.txt file

    Note in particular the **absolute ban** on using LLM or 'AI' tools, that **you are responsible** for the authenticity & confidentiality of your submission, and the **dire penalties** for trying to cheat.

* Submit through Brightspace
