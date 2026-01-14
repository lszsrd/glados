---
category: Functions
title: Functions
filename: function.rz
code: |
  fn square(Int: x) -> Int {
    ret x * x;
  }

  fn main() -> Int {
    Int result = square(5);
    ret result;
  }
---

# Functions

## Parameters

Functions can accept typed parameters. Each parameter has a type and a name, separated by commas.

## Return

Functions use `ret` to return a value. The return type is specified after `->`.

## Call

Functions are called using their name and arguments: `square(5)`
