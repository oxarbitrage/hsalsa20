import Quarterround

import Test.HUnit
import Keelung

main :: IO Counts
main = do
    -- Run tests

    putStrLn "Running Keelung quarterround tests:"
    putStrLn ""

    let quarterround_computed = quarterroundCompute [1, 0, 0, 0]
    putStrLn "Quarterround computed for input [1, 0, 0, 0]:"
    print quarterround_computed

    quarterround_interpreted <- interpret gf181 (quarterroundKeelung [1, 0, 0, 0]) [] []
    putStrLn "Quarterround simulated for input [1, 0, 0, 0]:"
    print quarterround_interpreted

    let quarterround_computed2 = quarterroundCompute [0xe7e8c006, 0xc4f9417d, 0x6479b4b2, 0x68c67137]
    putStrLn "Quarterround computed for input [0xe7e8c006, 0xc4f9417d, 0x6479b4b2, 0x68c67137]:"
    print quarterround_computed2

    quarterround_interpreted2 <- interpret gf181 (quarterroundKeelung [0xe7e8c006, 0xc4f9417d, 0x6479b4b2, 0x68c67137]) [] []
    putStrLn "Quarterround simulated for input [0xe7e8c006, 0xc4f9417d, 0x6479b4b2, 0x68c67137]:"
    print quarterround_interpreted2

    _quarterround_compiled <- compile bn128 (quarterroundKeelung [1, 0, 0, 0])
    --putStrLn "Quarterround compiled:"
    --print quarterround_compiled

    -- just return an empty `Count` so we don't have to return the one from a specific test:
    return (Counts 0 0 0 0)
