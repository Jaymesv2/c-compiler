# Language Ref

## Overview

This language is a simplified version of C.
It is loosely based on [ISO C99](https://www.dii.uchile.cl/~daespino/files/Iso_C_1999_definition.pdf).

I have modernized some parts of the language like how strings are handled, arrays (bounds checking), overflow semantics,

## Syntax

## Standard Types

There are no int, long, unsigned long long int, float, double, etc...
The standard int types are all of the matches for this regex `(i|u)(8|16|32|64)`. `i` for signed, `u` for unsigned
The float types are `f32` and `f64`.
Have a bool type ( 1 byte ).

## No ternary operator, assignment expression, comma expression, ++, -- operators

## function pointer types

specifiers/qualifiers, type, name.

c syntax for function pointers is horrible.

int\*()()

## Variable length arrays

no using "[*]" for a VLA

use a variable

## type casting

use "as" for type casts
ex "{expr} as {type}"

## Forward Declaration?

## structs. similar syntax to c.

# Plan

## 1. Lexical Analysis

The language should have a context free grammar.

## 2. Parse

Make a parse tree which is basically a 1-1 reflection of the source code.

## 3. Simplify the parse tree

Certain parts of the parse tree are expanded.
The specifiers and qualifiers are in a list and may have repeat elements.

## 4. Typechecking

There shouldn't be any type inference involved so it shouldn't be very difficult.

## 5. Compile to IR

## 6. Optimize the IR

## 7. Send the IR to a backend.

Either LLVM, cranelift, or a custom backend.

# BNF Grammar

## Type qualifiers

const, restrict, volatile.

## Function Specifiers

inline

##
