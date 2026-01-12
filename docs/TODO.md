# Syntax, grammar and semantic

- A formal document describing your language's grammar (you're encouraged to use BNF). This part is mandatory to validate.
    [OK] (./doc/BNF)
- A consistent, unclutered grammar. for example:
    * if your language generates more parenthesis than LISP, maybe you took a bad turn somewhere...
        [OK]
    * if your language use curly brackets and explicit return statement (just like C) but is otherwise not imperative, maybe there's a more concise way to represent your code.
        [OK]
- Some syntactic sugar (two (or more) syntactic forms which result in the same AST)
    [OK] (strings litteral, ternary operator)
- Infix arythmetic operators with the expected priorities (rembember the EvalExpr?)
    [OK]
- Infix user defined operators with user defined priority (as in Haskell or Kaleidoscope for example)

# Evaluation and compilation

- You must implement a Virtual Machine, with its own Instruction Set to execute your language programs.
    [OK] (./src/vm/types/OpCodes.hs)
- You must implement a Compiler capable of translating a program from your language into a form suitable for your VM (the compiler and VM can be two modules inside the same executable, or two binaries).
    [OK] (./glados-compiler and ./glados-vm)
- Your compiler should be capable of displaying its output in the form of human readable text (dissassembly).
- Your compiler should be able to output it's result as a binary format (bytecode).
- Your VM should be able to load this binary format and run it.
    [OK]
- Your could include in your delivery a standard library, coded in your language, extending the basic functionalities of your language.
- Your language could support closures.

# Documentation and accessibility

- A user manual, explaining how to use your language, with examples.
- A formal description of your language's grammar (you're encouraged to use BNF).
    [OK] (./doc/BNF/)
- A description of the compilation process.
- A review of the languages you took inspiration from, from a security point of view (or lack therof). - C (base, AST), Python (args des fonctions), Rust (fn, ->)
- A description of the security features you implemented, in relation with your review. - no self/non-initialized variables, no memory management
- Optionaly, a developer manual, explaining how to extend your language.

# Bonus

- More data types:
    * Floating point numbers (with corresponding division operator: / )
        [OK]
    * Symbols as data (as in LISP using quote, or as in Erlang/Elixir, or Prolog)
    * Lists
        * with corresponding builtins to manipulate them: cons/car/cdr or (:)/head/tail
        * syntactic sugar for literals, for example: [1,2,3]
            [OK]
    * Strings (as lists of Char or with a custom set of builtins to use them)
        [OK]
    * Tuples
    * Structures
    * Arrays
        [OK]
    * HashMaps
- Side effects:
    * add builtins or a notation to read or write to standard input / output
    * add builtins or a notation for file handling
    * bindings to graphical library, or networking primitives, etc.
    * a notation to interface with any external function (Foreign Function Interface)
- Type inference:
    * being able to detect before execution if a program / function is not properly typed
        [OK]
    * provide a notation to anotate functions / expressions with types
        [OK]
- Additionnal backends:
    * extend your compiler to output bytecode for an existing plateform (Java, C#, WebAssembly, Erlang, Python...)
    * extend your compiler to output native binary code for x86_64, ARM, RiscV, M68k, MOS-6502 or any real hardware (you can use bindings to LLVM to implement this, or handle the codegen part yourself)
- Additionnal runtime:
    * write a second VM in a different language (this will only be evaluated if the Haskell VM is fully functional)
- Metaprogrammation:
    * provide notations to allow the user to programmaticaly modify the program structure for example: LISP macros, Rust macros, Elixir macros...
- Imperative constructs:
    * capability to describe functions as sequence of statements
        [OK]
    * mutable variables
        [OK]
    * loops
        [OK]
- Optimisation
    * TCO (tail call optimisation)
    * anything that make your code run faster (don't forget the benchmarks to prove it!)
