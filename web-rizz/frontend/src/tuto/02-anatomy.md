---
category: Introduction
title: Anatomy of a function
filename: main.rz
code: |
  fn main() {
  }
---

# Anatomy of a function

These lines define a function named main. The main function is special: It is always the first code that runs in every executable Rizz program. Here, the first line declares a function named main that has no parameters and returns nothing. If there were parameters, they would go inside the parentheses `()`.

The function body is wrapped in `{}`. Rizz requires curly brackets around all function bodies. It's good style to place the opening curly bracket on the same line as the function declaration, adding one space in between.

We end statements with a semicolon `;`, which indicates that this expression is over, and the next one is ready to begin.

## Semicolons

- **Statements** end with a semicolon `;`
- **Blocks** (fn, if, for, while) do **not** require a semicolon after `}`
