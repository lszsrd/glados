## Language Inspirations

The **Rizz Compiler** draws inspiration from **three major programming languages**, each influencing different aspects of the language design and compiler architecture.

---

### C — Core Structure and AST Design

The foundational structure of the Rizz language is inspired by **C**, particularly its straightforward control flow and low-level expressiveness.
Additionally, the compiler’s internal **Abstract Syntax Tree (AST)** is heavily influenced by **Clang’s AST model**, providing a robust and well-structured representation of programs.

Example Rizz code:

```rizz
fn foo(Int: x) -> Int
{
    Int y = 42;

    for (Int a = 0; a < 10; a++) {
        y--;
    }

    if (x == 0) {
        println("Oh nyo, you tried dividing by 0");
        return -1;
    }

    ret y / x;
}
```

This example highlights familiar C-like constructs such as:

* Explicit variable declarations
* `for` loops and conditional statements
* Clear, imperative control flow

---

### Python — Function Argument Syntax

**Python** influenced the syntax used for function arguments in Rizz.
Function parameters are declared with explicit types, but use a clean and readable format:

```rizz
fn main(Int: x, Char: a) -> Int
```

This approach combines Python’s readability with static typing, keeping function signatures concise and expressive.

---

### Rust — Keywords and Return Type Syntax

**Rust** inspired several of Rizz’s keywords and syntactic conventions, most notably:

* The `fn` keyword for function declarations
* The return type annotation syntax using `->`

Example:

```rizz
fn compute(Int: value) -> Int
```

These choices improve clarity and make function definitions self-documenting.

---

## Safety and Design Principles

For **security and reliability reasons**, Rizz deliberately avoids exposing manual memory management to the user.

Key safety guarantees include:

* Users cannot directly manage memory
* **Every variable must be initialized** before use
* The compiler prevents access to uninitialized values at both compile time and runtime

These constraints help eliminate an entire class of common programming errors while keeping the language simple and predictable.
