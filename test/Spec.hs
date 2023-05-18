import Quarterround
import Rowround

import Data.Word

-- We define an alias for a 4-Tuple of Word32 objects.
type VectorType = (Word32, Word32, Word32, Word32)

-- We define an alias for a 16-Tuple of Word32 objects.
type MatrixType = (VectorType, VectorType, VectorType, VectorType)

-- Quarterround

quarterroundInput1 :: VectorType
quarterroundInput1 = (0, 0, 0, 0)

quarterroundOutput1 :: VectorType
quarterroundOutput1 = (0, 0, 0, 0)

quarterroundInput2 :: VectorType
quarterroundInput2 = (1, 0, 0, 0)

quarterroundOutput2 :: VectorType
quarterroundOutput2 = (0x08008145, 0x00000080, 0x00010200, 0x20500000)

-- Rowround

rowroundInput1 :: MatrixType
rowroundInput1 = (
    (1, 0, 0, 0), (1, 0, 0, 0), (1, 0, 0, 0), (1, 0, 0, 0))

rowroundOutput1 :: MatrixType
rowroundOutput1 = (
    (0x08008145, 0x00000080, 0x00010200, 0x20500000), (0x20100001, 0x00048044, 0x00000080, 0x00010000),
    (0x00000001, 0x00002000, 0x80040000, 0x00000000), (0x00000001, 0x00000200, 0x00402000, 0x88000100))

main :: IO ()
main = do
    putStrLn "Quarterround tests:"
    putStrLn $ if quarterround quarterroundInput1 == quarterroundOutput1 then "OK" else "FAIL!"
    putStrLn $ if quarterround quarterroundInput2 == quarterroundOutput2 then "OK" else "FAIL!"

    putStrLn "Rowround tests:"
    putStrLn $ if rowround rowroundInput1 == rowroundOutput1 then "OK" else "FAIL!"
  
    return ()
