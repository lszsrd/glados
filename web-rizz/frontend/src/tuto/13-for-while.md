---
category: Control Flow
title: For/While Loops
filename: loops.rz
code: |
  fn main() -> Int {
    Int sum = 0;
    Int i = 0;

    while (i < 5) {
      sum = sum + i;
      i = i + 1;
    }

    ret sum;
  }
---

# For/While Loops

## While loop

`while` repeats as long as the condition remains `True`.

## For loop

In a for loop, initialization, condition, and increment are separated by semicolons. The loop continues while the condition is `True`.
