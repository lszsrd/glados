---
category: Data Types
title: STR (Arrays)
filename: arrays.rz
code: |
  fn main() -> Int {
    [Int] x = [1, 2, 3];
    ret 0;
  }
---

# STR

A STR represents a typed collection of values (array).

```rizz
fn main() -> Int
{
  [Int] x = [1, 2, 3];
  ret 0;
}
```

`[Int]` declares an array of integers

- Values are listed inside `[]`
- All elements must have the same type
- Arrays must be initialized at declaration
