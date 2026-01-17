---
category: Introduction
title: Hello World
filename: hello.rz
code: |
  fn main() {
    print(1, "Hello, World!");
    Int x = 42;
    println(1, ("foo " + x) + (" bar" + " lol"));
  }
---

# Hello World!

Welcome to the Rizz tutorial! Let's start with the classic "Hello World" program.

The `print(fd, value)` function outputs text to a file descriptor (1 for stdout).

You can also concatenate strings and variables using the `+` operator.
