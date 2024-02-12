# Formal Verification of `hsalsa20` Cipher Implementation 

Ensuring the correctness of a cipher implementation involves rigorous testing and verification. While unit tests are effective for fixed inputs and property tests cover random values, ensuring program logic correctness requires a more systematic approach. [LiquidHaskell](https://github.com/ucsd-progsys/liquidhaskell) comes to the rescue, allowing us to annotate functions with properties that are rigorously checked for consistency.

## Leveraging LiquidHaskell for Confidence

LiquidHaskell facilitates the addition of annotations or refinements to function signatures, providing an extra layer of confidence in the implementation's correctness. These annotations are examined at compile time using the [z3](https://github.com/Z3Prover/z3) SMT solver. The resulting binaries maintain their original size, ensuring efficient verification without sacrificing performance.

## Properties Verification

LiquidHaskell allows us to verify various properties of our cipher implementation using liquid types.

### Safety Checks Categories

In our codebase, we have implemented several safety checks and plan to add more as needed.

#### Length of Input and Output Lists

Ensuring the lengths of input and output lists adhere to the protocol is a critical check. For example:

```haskell
{-@ doubleroundCompute :: { i:[_] | (len i) == 16 } -> { o:[_] | (len o) == 16 } @-}
doubleroundCompute :: [Word32] -> [Word32]
doubleroundCompute input
    | length input == 16 = rowroundCompute $ columnroundCompute input
    | otherwise = error "input to `doubleroundCompute` must be a list of 16 `Word32` numbers"
```

This refinement asserts that both input and output lists must have a length of 16, and the function ensures this requirement is met. We perform this check extensively in our codebase to ensure the consistency and correctness of our functions.

#### Equality of Elements in Transpose Function

The `transpose` function is used in implementing the columnround functionality in the `Columnround` module. Here we verify not only the length of lists (16) but also the equality of elements:

```haskell
{-@ transpose :: {i:[a] | len i == 16} -> {o:[a] | len o == 16 && (elts i == elts o) } @-}
transpose :: [a] -> [a]
```

This ensures that the transposition maintains element equality between input and output lists.

#### Equality of Elements in rowround sort functions

The `Rowround` module employs custom sort functions to shuffle the inputs sent to the `quarterround` functions, as specified in the salsa20 specification.

These sort functions simply shuffle input elements, ensuring that the same elements are returned in a different order. Similar to the approach used in the `transpose` function, we guarantee that the input and output elements remain the same. For example:

```haskell
{-@ sort2 :: { i:[_] | (len i) == 4 } -> { o:[_] | (len o) == 4 && (elts i == elts o) } @-}
sort2 :: [a] -> [a]
```

#### Retreiving a byte inside bounds

Within the `Crypt` module, we have a series of `keyByte` functions designed to extract specific bytes from a key, which is a list of bytes. These functions play a significant role in encryption and decryption processes, as outlined in the salsa20 specification.

The input length of these functions is fixed at 64. To ensure safety, we apply a refinement to validate that the index argument falls within the range of valid indices, from 0 to 63:

```haskell
{-@ keybyteCompute :: { i:[_] | (len i) == 64 } -> {index: Nat | index <= 63 } -> _  @-}
keybyteCompute :: [Word32] -> Int -> Word32
```

#### Concat64 Assumption

Assumptions are used to provide properties to functions, such as in the `concat64` function:

```haskell
{-@ assume concat64 :: {outer:[{inner:[a] | len inner == 4}] | len outer == 16} -> {output:[a] | len output == 64} @-}
concat64 :: [[a]] -> [a]
concat64 = concat
```

This assumption allows us to convey specific properties about input and output list sizes. The aument function demonstrates how this assumption is utilized:

```haskell
{-@ aument :: { i:[_] | (len i) == 16 } -> { o:[_] | (len o) == 64 } @-}
aument :: [Word32] -> [Word32]
aument input = concat64 [extractBytes4 x | x <- input]
```

LiquidHaskell can then validate that the assumptions made in `concat64` hold, proving that an input of 16 elements always results in an output of 64 elements.
