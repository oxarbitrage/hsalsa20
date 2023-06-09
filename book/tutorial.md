# Salsa20 analysis and computation

A library for the salsa20 protocol.

## Contents

- [Intro](#intro)
- [Expressions](#expressions)
- [Modules](#modules)
  - [Quaterround](#quaterround)
  - [Rowround](#rowround)
  - [Columnround](#columnround)
  - [Doubleround](#doubleround)
  - [Hash](#hash)
- [Analysis](#analysis)
  - [Known core collisions](#known-core-collisions)

## Intro

This software implement the encryption/decryption of messages using the [Salsa20 protocol](https://cr.yp.to/snuffle/spec.pdf). In addition, it allows type checking and analisys of each step of the cipher.

## Expressions

The salsa20 cipher is based (*almost) fully on 3 building block operations which are bitwise XOR (`⊕`), mod32 addition (`+`) and rotation (`<<<`).

The following are all valid expressions in our language that can be computed.

```
2 ⊕ ((1 + 4) <<< 7)

1 ⊕ ((4 ⊕ ((3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9) + 2 ⊕ ((1 + 4) <<< 7)) <<< 13) + 3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9)) <<< 18)

1 ⊕ 2 ⊕ 3 ⊕ 4 ⊕ 5

3 + 2 <<< 4

...
```

You can try this in `ghci`:

```
ghci> :load Quarterround
[1 of 2] Compiling Utils            ( Utils.hs, interpreted )
[2 of 2] Compiling Quarterround     ( Quarterround.hs, interpreted )
Ok, two modules loaded.
ghci> 16 ⊕ ((15 ⊕ ((14 ⊕ ((13 ⊕ ((16 + 15) <<< 7) + 16) <<< 9) + 13 ⊕ ((16 + 15) <<< 7)) <<< 13) + 14 ⊕ ((13 ⊕ ((16 + 15) <<< 7) + 16) <<< 9)) <<< 18)
1751885146
ghci> 
```

Variables can also be used and we will use this kind of expressions often:

```
ghci> y0 = 1 
ghci> y1 = 2
ghci> y3 = 3
ghci> y1 ⊕ ((y0 + y3) <<< 7)
514
ghci> 
```

If the variables are not defined then we will not be able to compute but they will be valid expressions for displaying.

* We said almost here because there are a few other operations that will be needed later, for example we will eventually need multiplication (`*`) when dealing with littleendian related expressions.  

## Modules

We separated the code project into modules following a similar structure of the [Salsa20 spec](https://cr.yp.to/snuffle/spec.pdf).

### Quarterround

A quarterround function will take a list of 4 numbera and produce an output of the same length.

For example:

```
ghci> :load Quarterround
[1 of 2] Compiling Utils            ( Utils.hs, interpreted )
[2 of 2] Compiling Quarterround     ( Quarterround.hs, interpreted )
Ok, two modules loaded.
ghci> input = [1, 2, 3, 4]
ghci> quarterroundCompute input
[2552136791,642,329219,2702221316]
ghci>
```

We can observe what's going on with `quarterroundDisplay`, by using the same input but as strings:

```
ghci> input = ["1", "2", "3", "4"]
ghci> quarterroundDisplay input
["1 \8853 ((4 \8853 ((3 \8853 ((2 \8853 ((1 + 4) <<< 7) + 1) <<< 9) + 2 \8853 ((1 + 4) <<< 7)) <<< 13) + 3 \8853 ((2 \8853 ((1 + 4) <<< 7) + 1) <<< 9)) <<< 18)","2 \8853 ((1 + 4) <<< 7)","3 \8853 ((2 \8853 ((1 + 4) <<< 7) + 1) <<< 9)","4 \8853 ((3 \8853 ((2 \8853 ((1 + 4) <<< 7) + 1) <<< 9) + 2 \8853 ((1 + 4) <<< 7)) <<< 13)"]
ghci>
```

The above does not looks very good, we will generally use `quarterroundEquations` instead with a bit of formatting:

```
ghci> mapM_ putStrLn $ quarterroundEquations input
z0 = 1 ⊕ ((4 ⊕ ((3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9) + 2 ⊕ ((1 + 4) <<< 7)) <<< 13) + 3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9)) <<< 18)
z1 = 2 ⊕ ((1 + 4) <<< 7)
z2 = 3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9)
z3 = 4 ⊕ ((3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9) + 2 ⊕ ((1 + 4) <<< 7)) <<< 13)
ghci> 
```

Above we have the 4 equations (1 per line) that will form a quarterround output. All those are valid expressions that can be computed as was shown in [expressions](#expressions).

This can not be computed but it can be useful in some cases:

```
ghci> input = ["y0", "y1", "y2", "y3"]
ghci> mapM_ putStrLn $ quarterroundEquations input
z0 = y0 ⊕ ((y3 ⊕ ((y2 ⊕ ((y1 ⊕ ((y0 + y3) <<< 7) + y0) <<< 9) + y1 ⊕ ((y0 + y3) <<< 7)) <<< 13) + y2 ⊕ ((y1 ⊕ ((y0 + y3) <<< 7) + y0) <<< 9)) <<< 18)
z1 = y1 ⊕ ((y0 + y3) <<< 7)
z2 = y2 ⊕ ((y1 ⊕ ((y0 + y3) <<< 7) + y0) <<< 9)
z3 = y3 ⊕ ((y2 ⊕ ((y1 ⊕ ((y0 + y3) <<< 7) + y0) <<< 9) + y1 ⊕ ((y0 + y3) <<< 7)) <<< 13)
ghci>
```

### Rowround

Once we know how to use the [Quarterround](#quarterround) module it is straightforward to use others like Rowround. Rowround requires a 16 numbers input list and can be easily be computed, for example:

```
ghci> :load Rowround
[1 of 3] Compiling Utils            ( Utils.hs, interpreted )
[2 of 3] Compiling Quarterround     ( Quarterround.hs, interpreted )
[3 of 3] Compiling Rowround         ( Rowround.hs, interpreted )
Ok, three modules loaded.
ghci> input = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
ghci> rowroundCompute input
[2552136791,642,329219,2702221316,1676795908,3895562229,1415,727560,1388041,2803015688,944151700,2700,3981,2046478,3912458252,1751885146]
ghci>
```

It's equations can be obtained for example as:

```
ghci> input = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"] 
ghci> mapM_ putStrLn $ rowroundEquations input
z0 = 1 ⊕ ((4 ⊕ ((3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9) + 2 ⊕ ((1 + 4) <<< 7)) <<< 13) + 3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9)) <<< 18)
z1 = 2 ⊕ ((1 + 4) <<< 7)
z2 = 3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9)
z3 = 4 ⊕ ((3 ⊕ ((2 ⊕ ((1 + 4) <<< 7) + 1) <<< 9) + 2 ⊕ ((1 + 4) <<< 7)) <<< 13)
z4 = 5 ⊕ ((8 ⊕ ((7 ⊕ ((6 + 5) <<< 7) + 6) <<< 9) + 7 ⊕ ((6 + 5) <<< 7)) <<< 13)
z5 = 6 ⊕ ((5 ⊕ ((8 ⊕ ((7 ⊕ ((6 + 5) <<< 7) + 6) <<< 9) + 7 ⊕ ((6 + 5) <<< 7)) <<< 13) + 8 ⊕ ((7 ⊕ ((6 + 5) <<< 7) + 6) <<< 9)) <<< 18)
z6 = 7 ⊕ ((6 + 5) <<< 7)
z7 = 8 ⊕ ((7 ⊕ ((6 + 5) <<< 7) + 6) <<< 9)
z8 = 9 ⊕ ((12 ⊕ ((11 + 10) <<< 7) + 11) <<< 9)
z9 = 10 ⊕ ((9 ⊕ ((12 ⊕ ((11 + 10) <<< 7) + 11) <<< 9) + 12 ⊕ ((11 + 10) <<< 7)) <<< 13)
z10 = 11 ⊕ ((10 ⊕ ((9 ⊕ ((12 ⊕ ((11 + 10) <<< 7) + 11) <<< 9) + 12 ⊕ ((11 + 10) <<< 7)) <<< 13) + 9 ⊕ ((12 ⊕ ((11 + 10) <<< 7) + 11) <<< 9)) <<< 18)
z11 = 12 ⊕ ((11 + 10) <<< 7)
z12 = 13 ⊕ ((16 + 15) <<< 7)
z13 = 14 ⊕ ((13 ⊕ ((16 + 15) <<< 7) + 16) <<< 9)
z14 = 15 ⊕ ((14 ⊕ ((13 ⊕ ((16 + 15) <<< 7) + 16) <<< 9) + 13 ⊕ ((16 + 15) <<< 7)) <<< 13)
z15 = 16 ⊕ ((15 ⊕ ((14 ⊕ ((13 ⊕ ((16 + 15) <<< 7) + 16) <<< 9) + 13 ⊕ ((16 + 15) <<< 7)) <<< 13) + 14 ⊕ ((13 ⊕ ((16 + 15) <<< 7) + 16) <<< 9)) <<< 18)
ghci> 
```

### Columnround

The columnround function described in the spec is just the rowround with a transposed input.

The functionality is the same as the [Rowround](#rowround) module.

We can compute values with `evalCompute`:

```
ghci> :load Columnround
[1 of 4] Compiling Utils            ( Utils.hs, interpreted )
[2 of 4] Compiling Quarterround     ( Quarterround.hs, interpreted )
[3 of 4] Compiling Rowround         ( Rowround.hs, interpreted )
[4 of 4] Compiling Columnround      ( Columnround.hs, interpreted )
Ok, four modules loaded.
ghci> input = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
ghci> columnroundCompute input
[811010494,75694083,1192963,3588,1797,2151944746,1201815557,1845256,920585,1034,3491831514,2260828175,3261186060,532494,2319,2690521958]
ghci>
```

And we can see the equations with `evalEquations`:

```
ghci> input = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"] 
ghci> mapM_ putStrLn $ columnroundEquations input
z0 = 1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18)
z1 = 2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13)
z2 = 3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9)
z3 = 4 ⊕ ((16 + 12) <<< 7)
z4 = 5 ⊕ ((1 + 13) <<< 7)
z5 = 6 ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) + 14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9)) <<< 18)
z6 = 7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13)
z7 = 8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9)
z8 = 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)
z9 = 10 ⊕ ((6 + 2) <<< 7)
z10 = 11 ⊕ ((7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) + 3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9)) <<< 18)
z11 = 12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13)
z12 = 13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13)
z13 = 14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9)
z14 = 15 ⊕ ((11 + 7) <<< 7)
z15 = 16 ⊕ ((12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) + 8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9)) <<< 18)
ghci> 
```

### Doubleround

Doubleround is simply columnround to the outoput of rowround. Computing is easy:

```
ghci> doubleroundCompute input
[1513634484,789111067,3217524413,217781214,3656453738,2566057262,1705957317,3855611843,3828125381,2955250501,3087749010,2521887335,3983801052,2753864981,335428412,555285485]
ghci>
```

So is displaying the equations:

```
ghci> input = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"] 
ghci> mapM_ putStrLn $ doubleroundEquations input
z0 = 1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18) ⊕ ((4 ⊕ ((16 + 12) <<< 7) ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) ⊕ ((1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18) + 4 ⊕ ((16 + 12) <<< 7)) <<< 7) + 1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18)) <<< 9) + 2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) ⊕ ((1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18) + 4 ⊕ ((16 + 12) <<< 7)) <<< 7)) <<< 13) + 3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) ⊕ ((1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18) + 4 ⊕ ((16 + 12) <<< 7)) <<< 7) + 1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18)) <<< 9)) <<< 18)
z1 = 2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) ⊕ ((1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18) + 4 ⊕ ((16 + 12) <<< 7)) <<< 7)
z2 = 3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) ⊕ ((1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18) + 4 ⊕ ((16 + 12) <<< 7)) <<< 7) + 1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18)) <<< 9)
z3 = 4 ⊕ ((16 + 12) <<< 7) ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) ⊕ ((1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18) + 4 ⊕ ((16 + 12) <<< 7)) <<< 7) + 1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18)) <<< 9) + 2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) ⊕ ((1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18) + 4 ⊕ ((16 + 12) <<< 7)) <<< 7)) <<< 13)
z4 = 5 ⊕ ((1 + 13) <<< 7) ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) ⊕ ((7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) ⊕ ((6 ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) + 14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9)) <<< 18) + 5 ⊕ ((1 + 13) <<< 7)) <<< 7) + 6 ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) + 14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9)) <<< 18)) <<< 9) + 7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) ⊕ ((6 ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) + 14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9)) <<< 18) + 5 ⊕ ((1 + 13) <<< 7)) <<< 7)) <<< 13)
z5 = 6 ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) + 14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9)) <<< 18) ⊕ ((5 ⊕ ((1 + 13) <<< 7) ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) ⊕ ((7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) ⊕ ((6 ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) + 14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9)) <<< 18) + 5 ⊕ ((1 + 13) <<< 7)) <<< 7) + 6 ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) + 14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9)) <<< 18)) <<< 9) + 7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) ⊕ ((6 ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) + 14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9)) <<< 18) + 5 ⊕ ((1 + 13) <<< 7)) <<< 7)) <<< 13) + 8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) ⊕ ((7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) ⊕ ((6 ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) + 14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9)) <<< 18) + 5 ⊕ ((1 + 13) <<< 7)) <<< 7) + 6 ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) + 14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9)) <<< 18)) <<< 9)) <<< 18)
z6 = 7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) ⊕ ((6 ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) + 14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9)) <<< 18) + 5 ⊕ ((1 + 13) <<< 7)) <<< 7)
z7 = 8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) ⊕ ((7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) ⊕ ((6 ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) + 14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9)) <<< 18) + 5 ⊕ ((1 + 13) <<< 7)) <<< 7) + 6 ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) + 14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9)) <<< 18)) <<< 9)
z8 = 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) ⊕ ((12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) ⊕ ((11 ⊕ ((7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) + 3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9)) <<< 18) + 10 ⊕ ((6 + 2) <<< 7)) <<< 7) + 11 ⊕ ((7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) + 3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9)) <<< 18)) <<< 9)
z9 = 10 ⊕ ((6 + 2) <<< 7) ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) ⊕ ((12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) ⊕ ((11 ⊕ ((7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) + 3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9)) <<< 18) + 10 ⊕ ((6 + 2) <<< 7)) <<< 7) + 11 ⊕ ((7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) + 3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9)) <<< 18)) <<< 9) + 12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) ⊕ ((11 ⊕ ((7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) + 3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9)) <<< 18) + 10 ⊕ ((6 + 2) <<< 7)) <<< 7)) <<< 13)
z10 = 11 ⊕ ((7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) + 3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9)) <<< 18) ⊕ ((10 ⊕ ((6 + 2) <<< 7) ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) ⊕ ((12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) ⊕ ((11 ⊕ ((7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) + 3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9)) <<< 18) + 10 ⊕ ((6 + 2) <<< 7)) <<< 7) + 11 ⊕ ((7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) + 3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9)) <<< 18)) <<< 9) + 12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) ⊕ ((11 ⊕ ((7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) + 3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9)) <<< 18) + 10 ⊕ ((6 + 2) <<< 7)) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) ⊕ ((12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) ⊕ ((11 ⊕ ((7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) + 3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9)) <<< 18) + 10 ⊕ ((6 + 2) <<< 7)) <<< 7) + 11 ⊕ ((7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) + 3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9)) <<< 18)) <<< 9)) <<< 18)
z11 = 12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) ⊕ ((11 ⊕ ((7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) + 3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9)) <<< 18) + 10 ⊕ ((6 + 2) <<< 7)) <<< 7)
z12 = 13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) ⊕ ((16 ⊕ ((12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) + 8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9)) <<< 18) + 15 ⊕ ((11 + 7) <<< 7)) <<< 7)
z13 = 14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) ⊕ ((16 ⊕ ((12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) + 8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9)) <<< 18) + 15 ⊕ ((11 + 7) <<< 7)) <<< 7) + 16 ⊕ ((12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) + 8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9)) <<< 18)) <<< 9)
z14 = 15 ⊕ ((11 + 7) <<< 7) ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) ⊕ ((16 ⊕ ((12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) + 8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9)) <<< 18) + 15 ⊕ ((11 + 7) <<< 7)) <<< 7) + 16 ⊕ ((12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) + 8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9)) <<< 18)) <<< 9) + 13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) ⊕ ((16 ⊕ ((12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) + 8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9)) <<< 18) + 15 ⊕ ((11 + 7) <<< 7)) <<< 7)) <<< 13)
z15 = 16 ⊕ ((12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) + 8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9)) <<< 18) ⊕ ((15 ⊕ ((11 + 7) <<< 7) ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) ⊕ ((16 ⊕ ((12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) + 8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9)) <<< 18) + 15 ⊕ ((11 + 7) <<< 7)) <<< 7) + 16 ⊕ ((12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) + 8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9)) <<< 18)) <<< 9) + 13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) ⊕ ((16 ⊕ ((12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) + 8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9)) <<< 18) + 15 ⊕ ((11 + 7) <<< 7)) <<< 7)) <<< 13) + 14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) ⊕ ((16 ⊕ ((12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) + 8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9)) <<< 18) + 15 ⊕ ((11 + 7) <<< 7)) <<< 7) + 16 ⊕ ((12 ⊕ ((8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9) + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) + 8 ⊕ ((4 ⊕ ((16 + 12) <<< 7) + 16) <<< 9)) <<< 18)) <<< 9)) <<< 18)
ghci>
```

Note how each equation grow as we build the cipher, this will make the `doubleround10` expression impractical to display so we have `doubleround2` as an alternative, which is the same but only doing doubleround 2 times instead of 10.

It is ok to compute doubleround10 with `doubleroundCompute`:

```
ghci> input = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
ghci> doubleround10Compute input
[1404140657,617196032,2156026738,1598378278,1320572932,2911795924,848484514,1720253541,2259298499,1446763212,3297210272,2542453700,1082050440,992640509,3623268805,2227813469]
ghci> 
```

But we will use `doubleround2` for displaying purposes and just get the first (`z0`) and second (`z1`) equations instead of the 16 total:

```
ghci> :load Doubleround
[1 of 5] Compiling Utils            ( Utils.hs, interpreted )
[2 of 5] Compiling Quarterround     ( Quarterround.hs, interpreted )
[3 of 5] Compiling Rowround         ( Rowround.hs, interpreted )
[4 of 5] Compiling Columnround      ( Columnround.hs, interpreted )
[5 of 5] Compiling Doubleround      ( Doubleround.hs, interpreted )
Ok, five modules loaded.
ghci> input = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"] 
ghci> mapM_ putStrLn $ [(doubleround2Equations input)!!0]
z0 = 1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18) ⊕ ((4 ⊕ ((16 + 12) <<< 7) ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) ⊕ ((1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18) + 4 ⊕ ((16 + 12) <<< 7)) <<< 7) + 1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18)) <<< 9) + 2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ... ⊕ ((1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18) + 4 ⊕ ((16 + 12) <<< 7)) <<< 7)) <<< 13) + 3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) ⊕ ((2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) ⊕ ((1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18) + 4 ⊕ ((16 + 12) <<< 7)) <<< 7) + 1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18)) <<< 9)) <<< 18)) <<< 9)) <<< 18)) <<< 9)) <<< 18)
ghci> mapM_ putStrLn $ [(doubleround2Equations input)!!1]
z1 = 2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) ⊕ ((1 ⊕ ((13 ⊕ ((9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9) + 5 ⊕ ((1 + 13) <<< 7)) <<< 13) + 9 ⊕ ((5 ⊕ ((1 + 13) <<< 7) + 1) <<< 9)) <<< 18) + 4 ⊕ ((16 + 12) <<< 7)) <<< 7) ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) ⊕ ... + 4 ⊕ ((16 + 12) <<< 7)) <<< 13) ⊕ ((11 ⊕ ((7 ⊕ ((3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9) + 15 ⊕ ((11 + 7) <<< 7)) <<< 13) + 3 ⊕ ((15 ⊕ ((11 + 7) <<< 7) + 11) <<< 9)) <<< 18) + 10 ⊕ ((6 + 2) <<< 7)) <<< 7)) <<< 7)) <<< 7)
ghci> 
```

You can observe that `z1` is shorter than `z0`. This is coming from the quarterround function and the fact that `z1` is the first computed value and can be done with the input only without depending of further calculations. We could state this is a lemma but we will not do that here.

### Hash

We have 2 functions inside this module. The `core` function is the core of the salsa20 cipher. It will execute all the above plus more. Let's get started with how that works.

Computing is easy, the `core` function accepts a list of 16 numbers and produce a result of the same type and length:

```
ghci> input = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
ghci> coreCompute input
[1404140658,617196034,2156026741,1598378282,1320572937,2911795930,848484521,1720253549,2259298508,1446763222,3297210283,2542453712,1082050453,992640523,3623268820,2227813485]
ghci>
```

Displaying `core` can be impractical when we use the underlying `doubleround10`, however we can use the reduced versions like `doubleround2` to see whats going on, for example:

```
ghci> input = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"] 
ghci> mapM_ putStrLn $ [(core2Equations input)!!1]
z1 = 2 ⊕ ((14 ⊕ ((10 ⊕ ((6 + 2) <<< 7) + 6) <<< 9) + 10 ⊕ ((6 + 2) <<< 7)) <<< 13) ... <<< 7)) <<< 7) + 2
```

The difference between the above expression and a `doubleround2` one is actually just the `+ 2` at the end, as `core` is defined to be a sum of each element of the `doubleround` output and the original input.

Now lets move on into the `salsa20` expressions where the input is a list of 64 objects instead. The 64 objects are reduced to 16 using `littleendian` and aumented back using `littleendian_inv`.

Computing is easy:

```
ghci> input = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64]
ghci> salsa20Compute input
[70,166,70,45,9,161,4,219,65,57,130,249,57,6,24,113,185,58,225,45,244,108,129,90,103,112,93,86,39,0,185,191,9,2,0,100,204,115,161,248,140,98,234,60,47,44,185,234,60,75,231,86,114,90,247,44,72,216,108,221,222,223,234,126]
```

Displaying will result in long strings even if we are using reduced number of doubleround rounds:

```
ghci> input2 = ["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64"]
ghci> mapM_ putStrLn $ [(salsa20Equations input2)!!1]
z1 = 8 >>> 1 + 2^8 * 2 + 2^16 * 3 + 2^24 * 4 ⊕ ((49 + 2^8 * 50 + 2^16 * 51 + 2^24 * 52 ⊕ ((33 + 2^8 * 34 + 2^16 * 35 + 2^24 * 36 ⊕ ((17 + 2^8 * 18 + 2^16 * 19 + 2^24 * 20 ⊕ ((1 + 2^8 * 2 + 2^16 * 3 + 2^24 * 4 + 49 + 2^8 * 50 + 2^16 * 51 + 2^24 * 52) <<< 7) + 1 + 2^8 * 2 + 2^16 * 3 + 2^24 * 4) <<< 9) + 17 + 2^8 * 18 + ... + 2^16 * 3 + 2^24 * 4 & 255
ghci>
```

We now have a few more operators, this are:

- `>>>` right shift.
- `*` multiplication.
- `^` power.
- `&` bitwise and.

The followings are valid expressions and will reduce to a number in ghci:

```
ghci> 2^16 * 3 & 1
65536
ghci> 2^16 * 3 + 2^24 * 4 & 255
67305472
ghci> 8 >>> 1 + 2^8 * 2
516
ghci>
```

## Analysis

### Known core collisions

We are going to try some things that can be found in the [On the Salsa20 Core Function](https://www.iacr.org/archive/fse2008/50860470/50860470.pdf). This paper was destroyed by Bernstein in [Response to “On the Salsa20 core function”](https://www.ecrypt.eu.org/stream/papersdir/2008/011.pdf) but it still work for us as an example of some properties the salsa20 cipher has.

So for example, the paper claims that quarterround is left invariant, where left invariant mean that the elements in the left of the square matrix will remain the same when fed with particular crafted data.

Lets check this out:

```
ghci> input = [1, -1, 1, -1]
ghci> quarterroundCompute input
[1,4294967295,1,4294967295]
ghci> input = [100, -100, 100, -100]
ghci> quarterroundCompute input
[100,4294967196,100,4294967196]
ghci> 
```

Lets analize the equations:

```
ghci> input = ["A", "-A", "A", "-A"]
ghci> mapM_ putStrLn $ quarterroundEquations input
z0 = A ⊕ ((-A ⊕ ((A ⊕ ((-A ⊕ ((A + -A) <<< 7) + A) <<< 9) + -A ⊕ ((A + -A) <<< 7)) <<< 13) + A ⊕ ((-A ⊕ ((A + -A) <<< 7) + A) <<< 9)) <<< 18)
z1 = -A ⊕ ((A + -A) <<< 7)
z2 = A ⊕ ((-A ⊕ ((A + -A) <<< 7) + A) <<< 9)
z3 = -A ⊕ ((A ⊕ ((-A ⊕ ((A + -A) <<< 7) + A) <<< 9) + -A ⊕ ((A + -A) <<< 7)) <<< 13)
ghci> 
```

From the above we can work out the invariance in `z0` and `z2`. Lets take `z2` as an example:

```
z2 = A ⊕ ((-A ⊕ ((A + -A) <<< 7) + A) <<< 9)
z2 = A ⊕ ((-A ⊕ (0 <<< 7) + A) <<< 9)
z2 = A ⊕ ((-A ⊕ 0 + A) <<< 9)
z2 = A ⊕ (0 <<< 9)
z2 = A ⊕ 0
z2 = A
```

We can have the machine do this workout for us if we use valid numbers. In `Word32` valid numbers are from 0 to 4294967295 so we can do the following:

```
ghci> input = ["1", "4294967295", "1", "4294967295"]
ghci> 4294967295 ⊕ ((1 + 4294967295) <<< 7)
4294967295
ghci> mapM_ putStrLn $ quarterroundEquations input
z0 = 1 ⊕ ((4294967295 ⊕ ((1 ⊕ ((4294967295 ⊕ ((1 + 4294967295) <<< 7) + 1) <<< 9) + 4294967295 ⊕ ((1 + 4294967295) <<< 7)) <<< 13) + 1 ⊕ ((4294967295 ⊕ ((1 + 4294967295) <<< 7) + 1) <<< 9)) <<< 18)
z1 = 4294967295 ⊕ ((1 + 4294967295) <<< 7)
z2 = 1 ⊕ ((4294967295 ⊕ ((1 + 4294967295) <<< 7) + 1) <<< 9)
z3 = 4294967295 ⊕ ((1 ⊕ ((4294967295 ⊕ ((1 + 4294967295) <<< 7) + 1) <<< 9) + 4294967295 ⊕ ((1 + 4294967295) <<< 7)) <<< 13)
ghci> 4294967295 ⊕ ((1 + 4294967295) <<< 7)
4294967295
ghci>
```

Following the paper, similar analysis can be done for `rowround`:

```
ghci> input = [1, -1, 1, -1, 2, -2, 2, -2, 3, -3, 3, -3, 4, -4, 4, -4]
ghci> rowroundCompute input
[1,4294967295,1,4294967295,2,4294967294,2,4294967294,3,4294967293,3,4294967293,4,4294967292,4,4294967292]
ghci> 
```

Or `columnround`:

```
ghci> input = [1, -2, 3, -4, -1, 2, -3, -4,1, -2, 3, -4, -1, 2, -3, -4]
ghci> columnroundCompute input
[1,4294967294,3,899,4294967295,2,4294967293,4294509052,1,4294967294,3,3746562051,4294967295,2,4294967293,4160783064]
ghci>
```

Or `doubleround`:

```
ghci> input = [1, -1, 1, -1, -1, 1, -1, 1, 1, -1, 1, -1, -1, 1, -1, 1]
ghci> doubleroundCompute input
[1,4294967295,1,4294967295,4294967295,1,4294967295,1,1,4294967295,1,4294967295,4294967295,1,4294967295,1]
ghci>
```

At the end we go after the `core` function and how two different non trivial inputs could lead to the same output.

Consider that:

```
2^31 = 2147483648
z = 2
z’ = 2 + 2147483648 = 2147483650
```

Then:

```
ghci> :load Hash
[1 of 6] Compiling Utils            ( Utils.hs, interpreted )
[2 of 6] Compiling Quarterround     ( Quarterround.hs, interpreted )
[3 of 6] Compiling Rowround         ( Rowround.hs, interpreted )
[4 of 6] Compiling Columnround      ( Columnround.hs, interpreted )
[5 of 6] Compiling Doubleround      ( Doubleround.hs, interpreted )
[6 of 6] Compiling Hash             ( Hash.hs, interpreted )
Ok, six modules loaded.
ghci> input1 = [2, -2, 2, -2, 2, -2, 2, -2, 2, -2, 2, -2, 2, -2, 2, -2] 
ghci> input2 = [2147483650, -2147483650, 2147483650, -2147483650, 2147483650, -2147483650, 2147483650, -2147483650, 2147483650, -2147483650, 2147483650, -2147483650, 2147483650, -2147483650, 2147483650, -2147483650] 
ghci> coreCompute input1
[3957682599,4247314016,3069266164,928241627,1833609583,2277935034,3608905336,2855561087,3069266164,928241627,3957682599,4247314016,3608905336,2855561087,1833609583,2277935034]
ghci> coreCompute input2
[3957682599,4247314016,3069266164,928241627,1833609583,2277935034,3608905336,2855561087,3069266164,928241627,3957682599,4247314016,3608905336,2855561087,1833609583,2277935034]
ghci>
```
