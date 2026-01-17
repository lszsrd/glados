---
category: Built-in Functions
title: Built-in Functions
filename: builtins.rz
code: |
  fn main() -> Int {
    Int x = rand(5, 100);
    dumpFile("LICENSE.md");
    println(1, ("factorial(" + x) + (") = " + factorial(x)));
    ret 0;
  }

  fn dumpFile ([Char]: path) {
    println(1, rand('a', 'z'));
    Int fd = open_r(path);

    if (isOpen(fd) == False) {
      exit(1);
    }
    while (isEOF(fd) == False) {
      println(1, readln(fd));
    }
    seek_e(fd, 0 - tell(fd));
    close(fd);
  }

  fn factorial(Int: x) -> Int {
    if (x == 1) {
      ret 1;
    }
    ret x * factorial(x - 1);
  }
---

# Built-in Functions

## Exit Functions

- `exit(Operand: )` → terminates execution and displays the operand as the "exit status"
- `cexit(Int: )` → binds to the C library exit

&nbsp;

## File Operations

- `open_r(String: )` → opens a file for reading and returns an Int (fd)
- `open_w(String: )` → opens a file for writing and returns an Int (fd)
- `open_a(String: )` → opens a file for writing without erasing its contents and returns an Int (fd)
- `open_rw(String: )` → opens a file for reading/writing and returns an Int (fd)
- `getFileSize(Int: )` → returns the file size as an Int (fd as parameter), or Int: -1 in case of error
- `isOpen(Int: )` → returns a Bool (True/False) indicating whether the given fd (Int) is open
- `close(Int: )` → closes the given fd and returns a Bool (True/False) (False if the fd is not mapped to an open file)
- `isEOF(Int: )` → returns a Bool (True/False) indicating whether the read head of the given fd (Int) is at end-of-file (fatal VM error if the fd is not valid)

&nbsp;

## I/O Operations

- `print(Int: , Operand: )` → writes the operand (any type) to the fd given as the first parameter (Int); fatal error if the fd is not mapped (the function returns nothing)
- `println(Int: , Operand: )` → writes the operand (any type) to the fd given as the first parameter (Int) and appends a newline; fatal error if the fd is not mapped (the function returns nothing)
- `read(Int: )` → reads ONE character from the given fd (Int) and returns it as a Char
- `readln(Int: )` → reads a line (equivalent to getline) from the given fd (Int) and returns a string ([Char])

&nbsp;

## File Position Operations

- `tell(Int: )` → returns the size of the given fd
- `seek_a(Int: , Int: )` → positions (seek equivalent) the read head of the fd (first parameter) to the absolute value given by the second parameter (from the beginning of the file)
- `seek_r(Int: , Int: )` → positions (seek equivalent) the read head of the fd (first parameter) to the value of the second parameter relative to the current position
- `seek_e(Int: , Int: )` → positions (seek equivalent) the read head of the fd (first parameter) to the value of the second parameter starting from the end of the file (to rewind a file to the beginning: `seek_e(fd, 0 - tell(fd));`)

&nbsp;

## Random

- `rand(Operand: , Operand: )` → returns a random value within the interval defined by the two arguments (supports only Bool & Bool, Char & Char, Int & Int)
