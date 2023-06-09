module Main (main) where

import Text.Printf
import Crypt (cryptBlock)

import Data.Word

message :: [Word32]
message = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0]

nonce :: [Word32]
nonce = [101, 102, 103, 104, 105, 106, 107, 108]

key :: [Word32]
key = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]

main :: IO ()
main = do
    putStrLn "Salsa20 in haskell - Category theory"

    let encrypted = cryptBlock message key nonce 0

    putStrLn $ printf "%d" $ head encrypted
    putStrLn $ printf "%d" $ encrypted!!1
    putStrLn $ printf "%d" $ encrypted!!2
    putStrLn $ printf "%d" $ encrypted!!3
    putStrLn $ printf "%d" $ encrypted!!4
    putStrLn $ printf "%d" $ encrypted!!5
    putStrLn $ printf "%d" $ encrypted!!6
    putStrLn $ printf "%d" $ encrypted!!7

    putStrLn $ printf "%d" $ encrypted!!63
    putStrLn $ printf "%d" $ encrypted!!64



    putStrLn ""

    let decrypted = cryptBlock encrypted key nonce 0

    putStrLn $ printf "%d" $ head decrypted
    putStrLn $ printf "%d" $ decrypted!!1
    putStrLn $ printf "%d" $ decrypted!!2
    putStrLn $ printf "%d" $ decrypted!!3
    putStrLn $ printf "%d" $ decrypted!!4
    putStrLn $ printf "%d" $ decrypted!!5
    putStrLn $ printf "%d" $ decrypted!!6
    putStrLn $ printf "%d" $ decrypted!!7


    return ()
