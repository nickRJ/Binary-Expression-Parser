# Binary Expression Parser

Blog post is [here](https://nickrj.me/2021/binary-expression-parsing-haskell.html).

## Overview

The expression parser transforms a string representing an expression into a binary tree. Here are some examples:

```
> putStrLn $ printExpr $ parseExpr "1*2"
*
├ 1
└ 2

> putStrLn $ printExpr $ parseExpr "2+4*6"
+
├ 2
└ *
  ├ 4
  └ 6
```

The parser supports unsigned integers, and the following operators:

```
Operator     Precedence      Associativity
------------------------------------------
* / %        5               Left to right
+ -          4               Left to right
&            3               Left to right
^            2               Left to right
|            1               Left to right
=            0               Right to left
```

These operators are a subset of the binary operators in C. They include multiplicative, additive, bit manipulation, and assignment operators. I’ve added the assignment operator since it’s right-associative, making our expression parser a bit more interesting.

Here's the grammar:

```
expression:   int-constant (op int-constant)*

int-constant: ('0' | ... | '9')+

op:           '*' | '/' | '%' | '+' | '-' | '&' | '^' | '|' | '='
```

## Setup

The code runs on ghc version `9.0.1`, though any version should do. The only third party library we need to have installed is [QuickCheck](https://hackage.haskell.org/package/QuickCheck), which is used to test the properties of the parser.

To run this code, fire up ghci and import Parser.hs

```bash
> ghci

ghci> :l parser
[1 of 1] Compiling Main             ( parser.hs, interpreted )
Ok, one module loaded.
```

To run any parser individually, use `runParser`:

```haskell
ghci> runParser int "123+4"
Right (123,"+4")

ghci> runParser op "+42"
Right (Op + 4 LFix,"42")
```

To parse entire expressions, use `parseExpr`:

```haskell
ghci> parseExpr "1+2"
BinExpr (Op + 4 LFix) (IntConst 1) (IntConst 2)
```

For more details on how this expression parser works, check out the blog post linked above. I've written a basic guide on how to build one entirely from scratch.
