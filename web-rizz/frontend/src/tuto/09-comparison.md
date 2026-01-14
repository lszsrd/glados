---
category: Operations
title: Comparison
filename: comparison.rz
code: |
  fn main() -> Int {
    Int x = 5;
    if (x == 5) {
      ret 1;
    }
    ret 0;
  }
---

# Comparison

Comparison expressions return a `Bool`.

| Operator | Meaning          |
| -------- | ---------------- |
| `==`     | equal            |
| `!=`     | not equal        |
| `<`      | less than        |
| `>`      | greater than     |
| `<=`     | less or equal    |
| `>=`     | greater or equal |
