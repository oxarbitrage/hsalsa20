import Quarterround

import Test.HUnit
import Keelung

main :: IO Counts
main = do
    -- Run tests

    putStrLn "Running Keelung quarterround tests:"

    quarterround_interpreted <- interpret bn128 (quarterroundKeelung [0, 1, 2, 3]) [] []
    putStrLn $ show quarterround_interpreted

    myexample2_interpreted <- interpret bn128 (quarterroundKeelung [11, 12, 13, 14]) [] []
    putStrLn $ show myexample2_interpreted

    myexample_compiled <- compile bn128 (quarterroundKeelung [1, 2, 3, 4])
    putStrLn $ show myexample_compiled

    -- just return an empty `Count` so we don't have to return the one from a specific test:
    return (Counts 0 0 0 0)
