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
- Produce a list of equations corresponding to rowround expressions.
- Perform Keelung specific computations using the UInt 32 type.

The rowround function is divided into four quarterround operations, each applying unique sorting strategies
to manipulate the input matrix.

-}
{-# LANGUAGE DataKinds #-}

module Rowround
    (
        rowroundCompute, rowroundDisplay, rowroundEquations, rowroundKeelung,
    )
where

import Text.Printf
import Data.Word
import Data.List.Split (chunksOf)

import Quarterround
import Utils

import Keelung hiding (input, eq)

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

-- |The algebra maps for computation.
algMapsCompute :: ExprF [Word32] -> [Word32]
algMapsCompute (Const i) = eitherListToNumberList i
algMapsCompute (Quarterround a) = Quarterround.quarterroundCompute a

-- |The algebra maps for displaying.
algMapsDisplay :: ExprF [String] -> [String]
algMapsDisplay (Const i) = map (printf "%s") (eitherListToStringList i)
algMapsDisplay (Quarterround a) = Quarterround.quarterroundDisplay a

-- |The algebra maps for Keelung.
algMapsKeelung :: ExprFKeelung (Comp [UInt 32]) -> Comp [UInt 32]
algMapsKeelung (ConstK i) =  return i
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
quarterround1 :: [Either Word32 String] -> Fix ExprF
quarterround1 a = In $ Quarterround $ In $ Const $ head $ chunksOf 4 a

-- |The first quarterround Keelung expression.
quarterround1Keelung :: [UInt 32] -> Fix ExprFKeelung
quarterround1Keelung a = In $ QuarterroundK $ In $ ConstK $ head $ chunksOf 4 a

-- |The second quarterround expression.
quarterround2 :: [Either Word32 String] -> Fix ExprF
quarterround2 a = In $ Quarterround $ In $ Const $ sort2 $ chunksOf 4 a!!1

-- |The second Keelung quarterround expression.
quarterround2Keelung :: [UInt 32] -> Fix ExprFKeelung
quarterround2Keelung a = In $ QuarterroundK $ In $ ConstK $ sort2 $ chunksOf 4 a!!1

-- |Sort a second input for rowround.
sort2 :: [a] -> [a]
sort2 [y4, y5, y6, y7] = [y5, y6, y7, y4]
sort2 _ = error "input to `sort2` must be a list of 4 objects"

-- |Inverse of `sort2`, used to order rowround output.
sort2_inv :: [a] -> [a]
sort2_inv [z5, z6, z7, z4] = [z4, z5, z6, z7] 
sort2_inv _ = error "input to `sort2_inv` must be a list of 4 objects"

-- |The third quarterround expression.
quarterround3 :: [Either Word32 String] -> Fix ExprF
quarterround3 a = In $ Quarterround $ In $ Const $ sort3 $ chunksOf 4 a!!2

-- |The third quarterround Keelung expression.
quarterround3Keelung :: [UInt 32] -> Fix ExprFKeelung
quarterround3Keelung a = In $ QuarterroundK $ In $ ConstK $ sort3 $ chunksOf 4 a!!2

-- |Sort a third input for rowround.
sort3 :: [a] -> [a]
sort3 [y8, y9, y10, y11] = [y10, y11, y8, y9] 
sort3 _ = error "input to `sort3` must be a list of 4 objects"

-- |Inverse of `sort3`, used to order rowround output.
sort3_inv :: [a] -> [a]
sort3_inv [z10, z11, z8, z9] = [z8, z9, z10, z11] 
sort3_inv _ = error "input to `sort3_inv` must be a list of 4 objects"

-- |The fourth quarterround expression.
quarterround4 :: [Either Word32 String] -> Fix ExprF
quarterround4 a = In $ Quarterround $ In $ Const $ sort4 $ chunksOf 4 a!!3

-- |The fourth Keelung quarterround expression.
quarterround4Keelung :: [UInt 32] -> Fix ExprFKeelung
quarterround4Keelung a = In $ QuarterroundK $ In $ ConstK $ sort4 $ chunksOf 4 a!!3

-- |Sort a fourth input for rowround.
sort4 :: [a] -> [a]
sort4 [y12, y13, y14, y15] = [y15, y12, y13, y14] 
sort4 _ = error "input to `sort4` must be a list of 4 objects"

-- |Inverse of `sort4`, used to order rowround output.
sort4_inv :: [a] -> [a]
sort4_inv [z15, z12, z13, z14] = [z12, z13, z14, z15]
sort4_inv _ = error "input to `sort4_inv` must be a list of 4 objects"

-- |The rowround expression computed.
rowroundCompute :: [Word32] -> [Word32]
rowroundCompute input
    | length input == 16 = concat [
        evalCompute $ quarterround1 $ numberListToEitherList input,
        sort2_inv $ evalCompute $ quarterround2 $ numberListToEitherList input,
        sort3_inv $ evalCompute $ quarterround3 $ numberListToEitherList input,
        sort4_inv $ evalCompute $ quarterround4 $ numberListToEitherList input]
    | otherwise = error "input to `rowroundCompute` must be a list of 16 `Word32` numbers"

-- |The rowround expression as a string.
rowroundDisplay :: [String] -> [String]
rowroundDisplay input
    | length input == 16 = concat [
        evalDisplay $ quarterround1 $ stringListToEitherList input,
        sort2_inv $ evalDisplay $ quarterround2 $ stringListToEitherList input,
        sort3_inv $ evalDisplay $ quarterround3 $ stringListToEitherList input,
        sort4_inv $ evalDisplay $ quarterround4 $ stringListToEitherList input]
    | otherwise = error "input to `rowroundCompute` must be a list of 16 `String` strings"

-- |The rowround expression as a list of equations.
rowroundEquations :: [String] -> IO ()
rowroundEquations input
    | length input == 16 = mapM_ putStrLn $ [printf "z%d = %s" (idx :: Int) eq | (idx, eq) <- zip [0..] (rowroundDisplay input)]
    | otherwise = error "input to `rowroundEquations` must be a list of 16 `String` strings"

-- |The rowround Keelung expression.
rowroundKeelung :: [UInt 32] -> Comp [UInt 32]
rowroundKeelung input
    | length input == 16 = do
        q1 <- evalKeelung $ quarterround1Keelung input
        q2 <- evalKeelung $ quarterround2Keelung input
        q3 <- evalKeelung $ quarterround3Keelung input
        q4 <- evalKeelung $ quarterround4Keelung input

        return $ concat [q1,  sort2_inv q2, sort3_inv q3, sort4_inv q4]
    | otherwise = error "input to `rowroundKeelung` must be a list of 16 `UInt 32` numbers"
