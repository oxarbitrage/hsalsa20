import Quarterround

import Test.HUnit

-- |Test success cases for the `quarterroundDisplay` and `quarterroundEquations` functions.
quarterroundDisplayTests :: Test
quarterroundDisplayTests = test
    ["quarterroundDisplay with valid input 1" ~:
        quarterroundDisplay ["y0", "y1", "y2", "y3"] ~?=
            [
                "y0 \8853 ((y3 \8853 ((y2 \8853 ((y1 \8853 ((y0 + y3) <<< 7) + y0) <<< 9) + y1 \8853 ((y0 + y3) <<< 7)) <<< 13) + y2 \8853 ((y1 \8853 ((y0 + y3) <<< 7) + y0) <<< 9)) <<< 18)",
                "y1 \8853 ((y0 + y3) <<< 7)",
                "y2 \8853 ((y1 \8853 ((y0 + y3) <<< 7) + y0) <<< 9)",
                "y3 \8853 ((y2 \8853 ((y1 \8853 ((y0 + y3) <<< 7) + y0) <<< 9) + y1 \8853 ((y0 + y3) <<< 7)) <<< 13)"
            ]
    ,"quarterroundEquations with valid input 1" ~:
        quarterroundEquations ["y0", "y1", "y2", "y3"] ~?=
            [
                "z0 = y0 ⊕ ((y3 ⊕ ((y2 ⊕ ((y1 ⊕ ((y0 + y3) <<< 7) + y0) <<< 9) + y1 ⊕ ((y0 + y3) <<< 7)) <<< 13) + y2 ⊕ ((y1 ⊕ ((y0 + y3) <<< 7) + y0) <<< 9)) <<< 18)",
                "z1 = y1 ⊕ ((y0 + y3) <<< 7)",
                "z2 = y2 ⊕ ((y1 ⊕ ((y0 + y3) <<< 7) + y0) <<< 9)","z3 = y3 ⊕ ((y2 ⊕ ((y1 ⊕ ((y0 + y3) <<< 7) + y0) <<< 9) + y1 ⊕ ((y0 + y3) <<< 7)) <<< 13)"
            ]
    ,"quarterroundDisplay with valid input 2" ~:
        quarterroundDisplay ["x0", "x1", "x2", "x3"] ~?=
            [
                "x0 \8853 ((x3 \8853 ((x2 \8853 ((x1 \8853 ((x0 + x3) <<< 7) + x0) <<< 9) + x1 \8853 ((x0 + x3) <<< 7)) <<< 13) + x2 \8853 ((x1 \8853 ((x0 + x3) <<< 7) + x0) <<< 9)) <<< 18)",
                "x1 \8853 ((x0 + x3) <<< 7)",
                "x2 \8853 ((x1 \8853 ((x0 + x3) <<< 7) + x0) <<< 9)",
                "x3 \8853 ((x2 \8853 ((x1 \8853 ((x0 + x3) <<< 7) + x0) <<< 9) + x1 \8853 ((x0 + x3) <<< 7)) <<< 13)"
            ]
    ,"quarterroundDisplay with valid input 3" ~:
        quarterroundDisplay ["1", "2", "3", "4"] ~?=
            [
                "1 \8853 ((4 \8853 ((3 \8853 ((2 \8853 ((1 + 4) <<< 7) + 1) <<< 9) + 2 \8853 ((1 + 4) <<< 7)) <<< 13) + 3 \8853 ((2 \8853 ((1 + 4) <<< 7) + 1) <<< 9)) <<< 18)",
                "2 \8853 ((1 + 4) <<< 7)",
                "3 \8853 ((2 \8853 ((1 + 4) <<< 7) + 1) <<< 9)",
                "4 \8853 ((3 \8853 ((2 \8853 ((1 + 4) <<< 7) + 1) <<< 9) + 2 \8853 ((1 + 4) <<< 7)) <<< 13)"
            ]
    ,"quarterroundDisplay with valid input 4" ~:
        quarterroundDisplay ["A", "A", "A", "A"] ~?=
            [
                "A \8853 ((A \8853 ((A \8853 ((A \8853 ((A + A) <<< 7) + A) <<< 9) + A \8853 ((A + A) <<< 7)) <<< 13) + A \8853 ((A \8853 ((A + A) <<< 7) + A) <<< 9)) <<< 18)",
                "A \8853 ((A + A) <<< 7)",
                "A \8853 ((A \8853 ((A + A) <<< 7) + A) <<< 9)",
                "A \8853 ((A \8853 ((A \8853 ((A + A) <<< 7) + A) <<< 9) + A \8853 ((A + A) <<< 7)) <<< 13)"
            ]
    ]

main :: IO Counts
main = do

    putStrLn "Running quarterround display tests:"
    runTestTT quarterroundDisplayTests
