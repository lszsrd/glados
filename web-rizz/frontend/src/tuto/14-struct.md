---
category: Data Types
title: STRUCT
filename: struct.rz
code: |
  struct Bar {
    Int: x,
    Int: y
  }

  fn main() -> Int {
    Bar->x = 4;
    ret 0;
  }
---

# STRUCT

struct allows you to define custom data types by grouping multiple values together.

```rizz
struct Bar {
  Int: x,
  Int: y
}
```

struct defines a new type

- Each field has a type and a name
- Fields are separated by new lines and a comma
- No semicolon is required after a struct declaration

## Field Access and Modification

```rizz
fn main() -> Int
{
  Bar->x = 4;
  ret 0;
}
```

The `->` operator is used to access struct fields

- Fields can be read or modified directly
