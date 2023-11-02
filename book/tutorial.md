# Salsa20 hash function algebra and computation

This tutorial covers the Salsa20 primitive core function, its algebraic representation, and the steps for computing it. 

It is inspired by the [Salsa20 protocol](https://cr.yp.to/snuffle/spec.pdf) and also provides insights into generating proofs for each construction step.

## Contents

- [Intro](#intro)
- [The language](#the-language)
  - [Variables](#variables)
  - [Constants](#constants)
  - [Symbols](#symbols)
  - [Operations](#operations)
- [Expressions](#expressions)
- [Modules](#modules)
  - [Quarterround](#quarterround)
  - [Rowround](#rowround)
  - [Columnround](#columnround)
  - [Doubleround](#doubleround)
  - [Core](#core)
- [Interpreting expressions](#interpreting-expressions)
- [Conclusions](#conclusions)

## Intro

The software implements the Salsa20 core function as described in the Salsa20 protocol. It also provides the ability to generate proofs for each step of the construction.

## The language

### Variables

Variables are represented using lower-case letters with optional indexes, e.g., `x0`, `x1`, `z1`, `z3`, `y99`, or simply `a`, `b`, `c`, etc.

### Constants

Constants can be decimal numbers (e.g., `7`, `0`, `199`) or hexadecimal numbers (e.g., `0x1ef8e9d3`, `0x53a8e4b5`).

### Symbols

Building blocks include:

- `⊕` : Bitwise XOR.
- `+` : Modulo 32 addition.
- `<<<` : Left shift.
- `(` : Left bracket.
- `)` : Right bracket.

Extended symbols not used in this tutorial include:

- `>>>` Right shift.
- `*` Multiplication.
- `^` Power.
- `&` Bitwise AND.

### Operations

Basic operations include:

- `a + b`
- `a ⊕ b`
- `a <<< s`

Extended operations (not used in this tutorial) involve:

- `a >>> s`
- `a * b`
- `2^a`
- `a & b`

Where `a`, `b` and `s` are variables.

## Expressions

The following are all well-formed expressions in our algebra:

```
2 ⊕ ((1 + 4) <<< 7)

1 ⊕ ((4 ⊕ ((3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9) + 2 ⊕ ((1 + 4) <<< 7)) <<< 13) + 3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9)) <<< 18)

1 ⊕ 2 ⊕ 3 ⊕ 4 ⊕ 5

3 + 2 <<< 4

a ⊕ (b + a) <<< 13

...
```

On the other hand the following expressions are not well-formed:

```
1 ⊕ ((

⊕⊕

⊕2

222 <<<)

CONST1 ⊕ CONST2
```

You can confirm their validity using `ghci`:

```
ghci> :load Operators
...
ghci> 16 ⊕ ((15 ⊕ ((14 ⊕ ((13 ⊕ ((16 + 15) <<< 7) + 16) <<< 9) + 13 ⊕ ((16 + 15) <<< 7)) <<< 13) + 14 ⊕ ((13 ⊕ ((16 + 15) <<< 7) + 16) <<< 9)) <<< 18)
1751885146
ghci> 1 ⊕ ((

<interactive>:3:7: error:
    parse error (possibly incorrect indentation or mismatched brackets)
ghci> ⊕⊕

<interactive>:4:1: error: parse error on input ‘⊕⊕’
ghci> ⊕ 2

<interactive>:5:1: error: parse error on input ‘⊕’
ghci> 1 ⊕ 2
3
ghci> 
```

In the above session, we first load the Operators file so `ghci` can parse operators that are not predefined, like `⊕` or `<<<`. Next, we compute a valid expression and test a few invalid ones.

## Modules

We separated the codebase into modules following a similar structure of the [Salsa20 spec](https://cr.yp.to/snuffle/spec.pdf).

### Quarterround

A quarterround function will take a list of 4 numbers and produce an output of the same length. For example:

```
ghci> :load Quarterround
...
ghci> input = [1, 2, 3, 4]
ghci> quarterroundCompute input
[2552136791,642,329219,2702221316]
ghci>
```

We can also view how each value is computed using `quarterroundDisplay`:

```
ghci> input = ["1", "2", "3", "4"]
ghci> quarterroundDisplay input
["1 \8853 ((4 \8853 ((3 \8853 ((2 \8853 ((1 + 4) <<< 7) + 1) <<< 9) + 2 \8853 ((1 + 4) <<< 7)) <<< 13) + 3 \8853 ((2 \8853 ((1 + 4) <<< 7) + 1) <<< 9)) <<< 18)","2 \8853 ((1 + 4) <<< 7)","3 \8853 ((2 \8853 ((1 + 4) <<< 7) + 1) <<< 9)","4 \8853 ((3 \8853 ((2 \8853 ((1 + 4) <<< 7) + 1) <<< 9) + 2 \8853 ((1 + 4) <<< 7)) <<< 13)"]
ghci>
```

For a more human-friendly format, we can display the 4 equations, one per line, which correspond to the `z1`, `z2`, and so on:

```
ghci> input = ["1", "2", "3", "4"]
ghci> mapM_ putStrLn $ quarterroundDisplay input
1 ⊕ ((4 ⊕ ((3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9) + 2 ⊕ ((1 + 4) <<< 7)) <<< 13) + 3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9)) <<< 18)
2 ⊕ ((1 + 4) <<< 7)
3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9)
4 ⊕ ((3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9) + 2 ⊕ ((1 + 4) <<< 7)) <<< 13)
ghci> 
```

These are valid Haskell expressions that can be computed, for example:

```
ghci> :load Operators
...
ghci> 4 ⊕ ((3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9) + 2 ⊕ ((1 + 4) <<< 7)) <<< 13)
2702221316
ghci> 
```

### Rowround

Once we understand how to use the Quarterround module, it's straightforward to use other modules like Rowround. Rowround operates on a 16-number list and can be computed as follows:

```
ghci> :load Rowround
...
ghci> input = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
ghci> rowroundCompute input
[2552136791,642,329219,2702221316,1676795908,3895562229,1415,727560,1388041,2803015688,944151700,2700,3981,2046478,3912458252,1751885146]
ghci>
```

The equations can be displayed as well:

```
ghci> input = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"] 
ghci> mapM_ putStrLn $ rowroundDisplay input
1 ⊕ ((4 ⊕ ((3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9) + 2 ⊕ ((1 + 4) <<< 7)) <<< 13) + 3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9)) <<< 18)
2 ⊕ ((1 + 4) <<< 7)
3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9)
4 ⊕ ((3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9) + 2 ⊕ ((1 + 4) <<< 7)) <<< 13)
5 ⊕ ((8 ⊕ ((7 ⊕ ((6 + 5) <<< 7) + 6) <<< 9) + 7 ⊕ ((6 + 5) <<< 7)) <<< 13)
6 ⊕ ((5 ⊕ ((8 ⊕ ((7 ⊕ ((6 + 5) <<< 7) + 6) <<< 9) + 7 ⊕ ((6 + 5) <<< 7)) <<< 13) + 8 ⊕ ((7 ⊕ ((6 + 5) <<< 7) + 6) <<< 9)) <<< 18)
7 ⊕ ((6 + 5) <<< 7)
8 ⊕ ((7 ⊕ ((6 + 5) <<< 7) + 6) <<< 9)
9 ⊕ ((12 ⊕ ((11 + 10) <<< 7) + 11) <<< 9)
10 ⊕ ((9 ⊕ ((12 ⊕ ((11 + 10) <<< 7) + 11) <<< 9) + 12 ⊕ ((11 + 10) <<< 7)) <<< 13)
11 ⊕ ((10 ⊕ ((9 ⊕ ((12 ⊕ ((11 + 10) <<< 7) + 11) <<< 9) + 12 ⊕ ((11 + 10) <<< 7)) <<< 13) + 9 ⊕ ((12 ⊕ ((11 + 10) <<< 7) + 11) <<< 9)) <<< 18)
12 ⊕ ((11 + 10) <<< 7)
13 ⊕ ((16 + 15) <<< 7)
14 ⊕ ((13 ⊕ ((16 + 15) <<< 7) + 16) <<< 9)
15 ⊕ ((14 ⊕ ((13 ⊕ ((16 + 15) <<< 7) + 16) <<< 9) + 13 ⊕ ((16 + 15) <<< 7)) <<< 13)
16 ⊕ ((15 ⊕ ((14 ⊕ ((13 ⊕ ((16 + 15) <<< 7) + 16) <<< 9) + 13 ⊕ ((16 + 15) <<< 7)) <<< 13) + 14 ⊕ ((13 ⊕ ((16 + 15) <<< 7) + 16) <<< 9)) <<< 18)
ghci>
```

### Columnround

The columnround function described in the spec is essentially a rowround applied to a transposed input. The concept is the same, but the input data is arranged differently. Let's see how Columnround works.

Computing is straightforward, just like with Rowround. The columnroundCompute function accepts a list of 16 numbers and produces a list of 16 new hashed numbers:

```
ghci> :load Columnround
...
ghci> input = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
ghci> columnroundCompute input
[811010494,75694083,1192963,3588,1797,2151944746,1201815557,1845256,920585,1034,3491831514,2260828175,3261186060,532494,2319,2690521958]
ghci>
```

Now, let's explore the equations used in the Columnround:

```
ghci> input = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"] 
ghci> mapM_ putStrLn $ columnroundDisplay input
1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18)
2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13)
3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9)
4 ⊕ ((16 + 12) <<< 7)
5 ⊕ ((1 + 13) <<< 7)
6 ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) + 14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9)) <<< 18)
7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13)
8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9)
9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)
10 ⊕ ((6 + 2) <<< 7)
11 ⊕ ((7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) + 3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9)) <<< 18)
12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13)
13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13)
14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9)
15 ⊕ ((11 + 7) <<< 7)
16 ⊕ ((12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) + 8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9)) <<< 18)
ghci> 
```

### Doubleround

The Doubleround combines the Columnround and Rowround functions to produce a new set of hashed values.

To compute the values, use the `doubleroundCompute` function. It takes a list of 16 numbers:
```
ghci> :load Doubleround
...
ghci> doubleroundCompute input
[1513634484,789111067,3217524413,217781214,3656453738,2566057262,1705957317,3855611843,3828125381,2955250501,3087749010,2521887335,3983801052,2753864981,335428412,555285485]
ghci>
```

To explore the equations, for brevity, here we display only the `z1` equation:

```
ghci> input = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"] 
ghci> mapM_ putStrLn $ [(doubleroundDisplay input)!!1]
2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) ⊕ ((1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18) + 4 ⊕ ((16 + 12) <<< 7)) <<< 7)
ghci> 
```

The `doubleroundR` function allows you to control the number of rounds.

The Doubleround is applied multiple times in the Salsa20 hash function, typically 10 rounds. However, you can adjust the number of rounds to create variations like salsa20/8 or salsa20/4, which use fewer or more rounds.

Here we compute with 10 rounds:

```
ghci> input = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
ghci> doubleroundRCompute input 10
[1404140657,617196032,2156026738,1598378278,1320572932,2911795924,848484514,1720253541,2259298499,1446763212,3297210272,2542453700,1082050440,992640509,3623268805,2227813469]
ghci> 
```

But we display two rounds:

```
ghci> :load Doubleround
...
ghci> input = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"] 
ghci> putStrLn (doubleroundRDisplay input 2!!1)
2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) ⊕ ((1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18) + 4 ⊕ ((16 + 12) <<< 7)) <<<
...
ghci>
```

### Core

The core functions are the essential primitives of the Salsa20 hash function. They execute all the previously mentioned modules and perform a final addition step to produce the hash. Let's explore how the Core function works.

To compute the hash, you can use the `coreCompute` function. It accepts a list of 16 numbers and the number of rounds to use in the underlying `doubleroundR` function, and it produces a list of 16 new hashed numbers:

```
ghci> :load Hash
...
ghci> input = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
ghci> coreCompute input 10
[1404140658,617196034,2156026741,1598378282,1320572937,2911795930,848484521,1720253549,2259298508,1446763222,3297210283,2542453712,1082050453,992640523,3623268820,2227813485]
ghci> 
```

Now, let's explore the equations used in the Core function. These equations are formed by applying the Doubleround function to the input multiple times (typically 10 rounds) and summing the results and the original input value. Here's an example of displaying the `z1` equation:

```
ghci> input = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"]
ghci> mapM_ putStrLn $ [(coreDisplay input 2)!!1]
2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) ⊕ ((1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18) + 4 ⊕ ((16 + 12) <<< 7)) <<< 7) ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 
...
<< 7)) <<< 7) + 2
```

Note the `+ 2` at the end, is the original value of the input which is being summed to all the doubleround term in the left. 

## Interpreting expressions

The expressions obtained from the `*Display` functions can be interpreted and evaluated to produce a numerical result. While we don't have a specific parser for this, you can use the [hint Haskell interpreter](https://hackage.haskell.org/package/hint) package to evaluate these expressions. Let's walk through how to interpret and evaluate expressions using this package.


Example: Evaluating `z1` equation from Rowround

```
ghci> :load Rowround
...
ghci> input = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"] 
ghci> let z1 = (rowroundDisplay input)!!1
ghci> import Language.Haskell.Interpreter
ghci> runInterpreter $ do { loadModules ["Operators.hs"]; setImports ["Prelude", "Operators"]; eval z1 }
Right "642"
ghci> 
```

Example: Evaluating `z7` equation from Doubleround

```
ghci> :load Doubleround
...
ghci> input = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"] 
ghci> let z1 = (doubleroundDisplay input)!!7
ghci> runInterpreter $ do { loadModules ["Operators.hs"]; setImports ["Prelude", "Operators"]; eval z1 }
Right "789111067"
ghci> 
```

## Conclusions

In this tutorial, we've explored the Salsa20 hash function, a cryptographic algorithm used for secure data hashing and encryption. We've broken down the Salsa20 protocol into its core components and provided a detailed explanation of how it works, along with practical examples.

We've introduced you to a specialized language for representing Salsa20 computations, including variables, constants, symbols, and operations. This language allows us to express the intricate operations involved in the Salsa20 hash function in a human-readable format.

Starting with the fundamental building blocks, such as the Quarterround, Rowround, Columnround, and Doubleround modules, we've shown how to compute and display the equations that make up these modules. These equations are essential for understanding how the Salsa20 algorithm processes data.

Additionally, we've explored the Core function, which combines the output of the Doubleround modules and performs a final addition to produce the hash. The Core function is the heart of the Salsa20 hash function and forms the basis for secure data hashing.

Finally, we've demonstrated how to interpret and evaluate the expressions generated by the *Display functions using the hint Haskell interpreter. This capability allows you to understand the numeric results produced at each step of the Salsa20 hash function, making it valuable for both learning and debugging purposes.

The Salsa20 hash function is a robust and secure algorithm, and by breaking it down into its individual components, you can gain a deep understanding of its inner workings. Whether you're interested in cryptography, data security, or simply want to learn about hashing algorithms, this tutorial provides you with a solid foundation to explore the Salsa20 protocol further.

Now that you have a grasp of the Salsa20 hash function's key elements, you can delve deeper into its applications and explore how it can be used to secure data and communications in various contexts.

Happy hashing and encrypting!

