import Quarterround

import Test.HUnit
import Keelung

import Data.List.Split (chunksOf)
import Data.Word

import Data.Bits

boolListToWord32 :: [Bool] -> Word32
boolListToWord32 bools = foldl (\acc b -> acc `seq` ((acc `Data.Bits.shiftL` 1) Data.Bits..|. fromIntegral (fromEnum b))) 0 bools

listsOfIntsToBool :: [[Integer]] -> [[Bool]]
listsOfIntsToBool = map (map (/= 0))

main :: IO Counts
main = do
    -- Run tests

    putStrLn "Running Keelung quarterround tests:"
    putStrLn ""

    let quarterround_computed = quarterroundCompute [1, 0, 0, 0]
    putStrLn "Quarterround computed:"
    print quarterround_computed

    quarterround2_interpreted <- interpret gf181 (quarterroundKeelung [1, 0, 0, 0]) [] []
    let list_of_ints = chunksOf 32 quarterround2_interpreted
    let list_of_bools = listsOfIntsToBool list_of_ints

    let list_of_words = [boolListToWord32 (list_of_bools!!0), boolListToWord32 (list_of_bools!!1),
            boolListToWord32 (list_of_bools!!2), boolListToWord32 (list_of_bools!!3)]
    putStrLn "Quarterround simulated:"
    print list_of_words

    _quarterround_compiled <- compile bn128 (quarterroundKeelung [0, 0, 0, 0])
    --putStrLn $ show quarterround_compiled

    -- just return an empty `Count` so we don't have to return the one from a specific test:
    return (Counts 0 0 0 0)
