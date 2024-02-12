{-|
Module      : Rowround
Description : Implementation of the Salsa20 stream cipher rowround expressions.
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

This module implements the rowround function, a fundamental operation in the Salsa20 stream cipher.
The rowround function processes a 4x4 matrix, employing the quarterround function as its single operation.
The quarterround expressions are formed and evaluated through F-Algebra operations, including modular arithmetic,
rotation, and bitwise XOR.

The module offers functionalities to:

- Compute numeric values resulting from rowround expressions.
- Generate string representations of rowround expressions.
- Perform Keelung specific computations using the UInt 32 type.

The rowround function is divided into four quarterround operations, each applying unique sorting strategies
to manipulate the input matrix.

-}
{-@ LIQUID "--no-positivity-check" @-}
{-@ LIQUID "--prune-unsorted" @-}
{-@ LIQUID "--no-termination" @-}
{-# LANGUAGE DataKinds #-}

module Rowround
    (
        rowroundCompute, rowroundDisplay, rowroundKeelung,
    )
where

import Text.Printf
import Data.Word

import Quarterround
import Utils

import Keelung ( UInt, Comp )

-- |The rowround endofunctor for Haskell types.
data ExprF a = Const [Either Word32 String] | Quarterround a

-- |The rowround endofunctor for Keelung types.
data ExprFKeelung a = ConstK [UInt 32] | QuarterroundK a

-- |Functor instance for Haskell types.
instance Functor ExprF where
    fmap _ (Const i) = Const i
    fmap f (Quarterround a) = Quarterround (f a)

-- |Functor instance for Keelung types.
instance Functor ExprFKeelung where
    fmap _ (ConstK i) = ConstK i
    fmap f (QuarterroundK a) = QuarterroundK (f a)

-- |Fix and unFix.
newtype Fix f = In (f (Fix f))
unFix :: Fix f -> f (Fix f)
unFix (In x) = x

-- |The catamorphism.
cata :: Functor f => (f a -> a) -> Fix f -> a
cata algebra = algebra . fmap (cata algebra) . unFix

{-| The algebra maps for computation.

TODO: The `otherwise` case silently return `[0, 0, 0, 0]` instead of erroring out.
-}
{-@ algMapsCompute :: _ -> { o:[_] | (len o) == 4 } @-}
algMapsCompute :: ExprF [Word32] -> [Word32]
algMapsCompute (Const i)
    | length i == 4 = eitherListToNumberList i
    | otherwise = [0, 0, 0, 0]
algMapsCompute (Quarterround a) = Quarterround.quarterroundCompute a

{-| The algebra maps for displaying.

TODO: The `otherwise` case silently return `["0", "0", "0", "0"]` instead of erroring out.
-}
{-@ algMapsDisplay :: _ -> { o:[_] | (len o) == 4 } @-}
algMapsDisplay :: ExprF [String] -> [String]
algMapsDisplay (Const i)
    | length i == 4 = map (printf "%s") (eitherListToStringList i)
    | otherwise = ["0", "0", "0", "0"]
algMapsDisplay (Quarterround a) = Quarterround.quarterroundDisplay a

{-| The algebra maps for Keelung.

TODO: The `otherwise` case silently return `[0, 0, 0, 0]` instead of erroring out.
-}
{-@ algMapsKeelung :: _ -> Comp { o:[_] | (len o) == 4 } @-}
algMapsKeelung :: ExprFKeelung (Comp [UInt 32]) -> Comp [UInt 32]
algMapsKeelung (ConstK i)
    | length i == 4 = return i
    | otherwise = return [0, 0, 0, 0]
algMapsKeelung (QuarterroundK a) = Quarterround.quarterroundKeelung =<< a

-- |The rowround evaluator.
evalCompute :: Fix ExprF -> [Word32]
evalCompute = cata algMapsCompute

-- |The rowround evaluator as a string.
evalDisplay :: Fix ExprF -> [String]
evalDisplay = cata algMapsDisplay

-- |The rowround evaluator as a Keelung expression.
evalKeelung :: Fix ExprFKeelung -> Comp [UInt 32]
evalKeelung = cata algMapsKeelung

-- |The first quarterround expression.
{-@ quarterround1 :: { i:[_] | (len i) == 16 } -> _ @-}
quarterround1 :: [Either Word32 String] -> Fix ExprF
quarterround1 [a0, a1, a2, a3, _, _, _, _, _, _, _, _, _, _, _, _] = In $ Quarterround $ In $ Const [a0, a1, a2, a3]
quarterround1 _ = error "input to `quarterround1` must be a list of 16 `Either` objects"

-- |The first quarterround Keelung expression.
{-@ quarterround1Keelung :: { v:[_] | (len v) == 16 } -> _ @-}
quarterround1Keelung :: [UInt 32] -> Fix ExprFKeelung
quarterround1Keelung [a0, a1, a2, a3, _, _, _, _, _, _, _, _, _, _, _, _] =  In $ QuarterroundK $ In $ ConstK [a0, a1, a2, a3]
quarterround1Keelung _ = error "input to `quarterround1Keelung` must be a list of 16 `UInt 32` numbers"

-- |The second quarterround expression.
{-@ quarterround2 :: { v:[_] | (len v) == 16 } -> _ @-}
quarterround2 :: [Either Word32 String] -> Fix ExprF
quarterround2 [_, _, _, _, a4, a5, a6, a7, _, _, _, _, _, _, _, _] = In $ Quarterround $ In $ Const $ sort2 [a4, a5, a6, a7]
quarterround2 _ = error "input to `quarterround2` must be a list of 16 `Either` objects"

-- |The second Keelung quarterround expression.
{-@ quarterround2Keelung :: { v:[_] | (len v) == 16 } -> _ @-}
quarterround2Keelung :: [UInt 32] -> Fix ExprFKeelung
quarterround2Keelung [_, _, _, _, a4, a5, a6, a7, _, _, _, _, _, _, _, _] = In $ QuarterroundK $ In $ ConstK $ sort2 [a4, a5, a6, a7]
quarterround2Keelung _ = error "input to `quarterround2Keelung` must be a list of 16 `UInt 32` numbers"

-- |Sort a second input for rowround.
{-@ sort2 :: { i:[_] | (len i) == 4 } -> { o:[_] | (len o) == 4 && (elts i == elts o) } @-}
sort2 :: [a] -> [a]
sort2 [y4, y5, y6, y7] = [y5, y6, y7, y4]
sort2 _ = error "input to `sort2` must be a list of 4 objects"

-- |Inverse of `sort2`, used to order rowround output.
{-@ sort2_inv :: { i:[_] | (len i) == 4 } -> { o:[_] | (len o) == 4 && (elts i == elts o) } @-}
sort2_inv :: [a] -> [a]
sort2_inv [z5, z6, z7, z4] = [z4, z5, z6, z7] 
sort2_inv _ = error "input to `sort2_inv` must be a list of 4 objects"

-- |The third quarterround expression.
{-@ quarterround3 :: { v:[_] | (len v) == 16 } -> _ @-}
quarterround3 :: [Either Word32 String] -> Fix ExprF
quarterround3 [_, _, _, _, _, _, _, _, a8, a9, a10, a11, _, _, _, _] = In $ Quarterround $ In $ Const $ sort3 [a8, a9, a10, a11]
quarterround3 _ = error "input to `quarterround3` must be a list of 16 `Either` objects"

-- |The third quarterround Keelung expression.
{-@ quarterround3Keelung :: { v:[_] | (len v) == 16 } -> _ @-}
quarterround3Keelung :: [UInt 32] -> Fix ExprFKeelung
quarterround3Keelung [_, _, _, _, _, _, _, _, a8, a9, a10, a11, _, _, _, _] = In $ QuarterroundK $ In $ ConstK $ sort3 [a8, a9, a10, a11]
quarterround3Keelung _ = error "input to `quarterround3Keelung` must be a list of 16 `UInt 32` numbers"

-- |Sort a third input for rowround.
{-@ sort3 :: { i:[_] | (len i) == 4 } -> { o:[_] | (len o) == 4 && (elts i == elts o) } @-}
sort3 :: [a] -> [a]
sort3 [y8, y9, y10, y11] = [y10, y11, y8, y9] 
sort3 _ = error "input to `sort3` must be a list of 4 objects"

-- |Inverse of `sort3`, used to order rowround output.
{-@ sort3_inv :: { i:[_] | (len i) == 4 } -> { o:[_] | (len o) == 4 && (elts i == elts o) } @-}
sort3_inv :: [a] -> [a]
sort3_inv [z10, z11, z8, z9] = [z8, z9, z10, z11]
sort3_inv _ = error "input to `sort3_inv` must be a list of 4 objects"

-- |The fourth quarterround expression.
{-@ quarterround4 :: { v:[_] | (len v) == 16 } -> _ @-}
quarterround4 :: [Either Word32 String] -> Fix ExprF
quarterround4 [_, _, _, _, _, _, _, _, _, _, _, _, a12, a13, a14, a15] = In $ Quarterround $ In $ Const $ sort4 [a12, a13, a14, a15]
quarterround4 _ = error "input to `quarterround4` must be a list of 16 `Either` objects"

-- |The fourth Keelung quarterround expression.
{-@ quarterround4Keelung :: { v:[_] | (len v) == 16 } -> _ @-}
quarterround4Keelung :: [UInt 32] -> Fix ExprFKeelung
quarterround4Keelung [_, _, _, _, _, _, _, _, _, _, _, _, a12, a13, a14, a15] = In $ QuarterroundK $ In $ ConstK $ sort4 [a12, a13, a14, a15]
quarterround4Keelung _ = error "input to `quarterround4Keelung` must be a list of 16 `UInt 32` numbers"

-- |Sort a fourth input for rowround.
{-@ sort4 :: { i:[_] | (len i) == 4 } -> { o:[_] | (len o) == 4 && (elts i == elts o) } @-}
sort4 :: [a] -> [a]
sort4 [y12, y13, y14, y15] = [y15, y12, y13, y14] 
sort4 _ = error "input to `sort4` must be a list of 4 objects"

-- |Inverse of `sort4`, used to order rowround output.
{-@ sort4_inv :: { i:[_] | (len i) == 4 } -> { o:[_] | (len o) == 4 && (elts i == elts o) } @-}
sort4_inv :: [a] -> [a]
sort4_inv [z15, z12, z13, z14] = [z12, z13, z14, z15]
sort4_inv _ = error "input to `sort4_inv` must be a list of 4 objects"

-- |The rowround expression computed.
{-@ rowroundCompute :: { i:[_] | (len i) == 16 } -> { o:[_] | (len o) == 16 }  @-}
rowroundCompute :: [Word32] -> [Word32]
rowroundCompute input
    | length input == 16 =
        evalCompute (quarterround1 $ map Left input) ++
        sort2_inv (evalCompute $ quarterround2 $ map Left input) ++
        sort3_inv (evalCompute $ quarterround3 $ map Left input) ++
        sort4_inv (evalCompute $ quarterround4 $ map Left input)
    | otherwise = error "input to `rowroundCompute` must be a list of 16 `Word32` numbers"

-- |The rowround expression as a string.
{-@ rowroundDisplay :: { i:[_] | (len i) == 16 } -> { o:[_] | (len o) == 16 }  @-}
rowroundDisplay :: [String] -> [String]
rowroundDisplay input
    | length input == 16 =
        evalDisplay (quarterround1 $ map Right input) ++
        sort2_inv (evalDisplay $ quarterround2 $ map Right input) ++
        sort3_inv (evalDisplay $ quarterround3 $ map Right input) ++
        sort4_inv (evalDisplay $ quarterround4 $ map Right input)
    | otherwise = error "input to `rowroundDisplay` must be a list of 16 `String` strings"

-- |The rowround Keelung expression.
{-@ rowroundKeelung :: { i:[_] | (len i) == 16 } -> Comp { o:[_] | (len o) == 16 }  @-}
rowroundKeelung :: [UInt 32] -> Comp [UInt 32]
rowroundKeelung input
    | length input == 16 = do
        q1 <- evalKeelung $ quarterround1Keelung input
        q2 <- evalKeelung $ quarterround2Keelung input
        q3 <- evalKeelung $ quarterround3Keelung input
        q4 <- evalKeelung $ quarterround4Keelung input

        let res = q1 ++ sort2_inv q2 ++ sort3_inv q3 ++ sort4_inv q4
        return res
    | otherwise = error "input to `rowroundKeelung` must be a list of 16 `UInt 32` numbers"
