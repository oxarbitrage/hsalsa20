import Quarterround
import Rowround
import Columnround
import Doubleround

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

quarterroundInput3 :: VectorType
quarterroundInput3 = (0, 1, 0, 0)

quarterroundOutput3 :: VectorType
quarterroundOutput3 = (0x88000100, 0x00000001, 0x00000200, 0x00402000)

quarterroundInput4 :: VectorType
quarterroundInput4 = (0, 0, 1, 0)

quarterroundOutput4 :: VectorType
quarterroundOutput4 = (0x80040000, 0x00000000, 0x00000001, 0x00002000)

quarterroundInput5 :: VectorType
quarterroundInput5 = (0, 0, 0, 1)

quarterroundOutput5 :: VectorType
quarterroundOutput5 = (0x00048044, 0x00000080, 0x00010000, 0x20100001)

quarterroundInput6 :: VectorType
quarterroundInput6 = (0xe7e8c006, 0xc4f9417d, 0x6479b4b2, 0x68c67137)

quarterroundOutput6 :: VectorType
quarterroundOutput6 = (0xe876d72b, 0x9361dfd5, 0xf1460244, 0x948541a3)

-- Rowround

rowroundInput1 :: MatrixType
rowroundInput1 = (
    (1, 0, 0, 0), (1, 0, 0, 0), (1, 0, 0, 0), (1, 0, 0, 0))

rowroundOutput1 :: MatrixType
rowroundOutput1 = (
    (0x08008145, 0x00000080, 0x00010200, 0x20500000), (0x20100001, 0x00048044, 0x00000080, 0x00010000),
    (0x00000001, 0x00002000, 0x80040000, 0x00000000), (0x00000001, 0x00000200, 0x00402000, 0x88000100))


rowroundInput2 :: MatrixType
rowroundInput2 = (
    (0x08521bd6, 0x1fe88837, 0xbb2aa576, 0x3aa26365), (0xc54c6a5b, 0x2fc74c2f, 0x6dd39cc3, 0xda0a64f6),
    (0x90a2f23d, 0x067f95a6, 0x06b35f61, 0x41e4732e), (0xe859c100, 0xea4d84b7, 0x0f619bff, 0xbc6e965a))

rowroundOutput2 :: MatrixType
rowroundOutput2 = (
    (0xa890d39d, 0x65d71596, 0xe9487daa, 0xc8ca6a86), (0x949d2192, 0x764b7754, 0xe408d9b9, 0x7a41b4d1),
    (0x3402e183, 0x3c3af432, 0x50669f96, 0xd89ef0a8), (0x0040ede5, 0xb545fbce, 0xd257ed4f, 0x1818882d))

-- Columnround

columnroundInput1 :: MatrixType
columnroundInput1 = (
    (1, 0, 0, 0), (1, 0, 0, 0), (1, 0, 0, 0), (1, 0, 0, 0))

columnroundOutput1 :: MatrixType
columnroundOutput1 = (
    (0x10090288, 0x00000000, 0x00000000, 0x00000000), (0x00000101, 0x00000000, 0x00000000, 0x00000000),
    (0x00020401, 0x00000000, 0x00000000, 0x00000000), (0x40a04001, 0x00000000, 0x00000000, 0x00000000))

columnroundInput2 :: MatrixType
columnroundInput2 = (
    (0x08521bd6, 0x1fe88837, 0xbb2aa576, 0x3aa26365), (0xc54c6a5b, 0x2fc74c2f, 0x6dd39cc3, 0xda0a64f6),
    (0x90a2f23d, 0x067f95a6, 0x06b35f61, 0x41e4732e), (0xe859c100, 0xea4d84b7, 0x0f619bff, 0xbc6e965a))

columnroundOutput2 :: MatrixType
columnroundOutput2 = (
    (0x8c9d190a, 0xce8e4c90, 0x1ef8e9d3, 0x1326a71a), (0x90a20123, 0xead3c4f3, 0x63a091a0, 0xf0708d69),
    (0x789b010c, 0xd195a681, 0xeb7d5504, 0xa774135c), (0x481c2027, 0x53a8e4b5, 0x4c1f89c5, 0x3f78c9c8))

-- Doubleround

doubleroundInput1 :: MatrixType
doubleroundInput1 = (
    (1, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0))

doubleroundOutput1 :: MatrixType
doubleroundOutput1 = (
    (0x8186a22d, 0x0040a284, 0x82479210, 0x06929051), (0x08000090, 0x02402200, 0x00004000, 0x00800000),
    (0x00010200, 0x20400000, 0x08008104, 0x00000000), (0x20500000, 0xa0000040, 0x0008180a, 0x612a8020))

doubleroundInput2 :: MatrixType
doubleroundInput2 = (
    (0xde501066, 0x6f9eb8f7, 0xe4fbbd9b, 0x454e3f57), (0xb75540d3, 0x43e93a4c, 0x3a6f2aa0, 0x726d6b36),
    (0x9243f484, 0x9145d1e8, 0x4fa9d247, 0xdc8dee11), (0x054bf545, 0x254dd653, 0xd9421b6d, 0x67b276c1))

doubleroundOutput2 :: MatrixType
doubleroundOutput2 = (
    (0xccaaf672, 0x23d960f7, 0x9153e63a, 0xcd9a60d0), (0x50440492, 0xf07cad19, 0xae344aa0, 0xdf4cfdfc),
    (0xca531c29, 0x8e7943db, 0xac1680cd, 0xd503ca00), (0xa74b2ad6, 0xbc331c5c, 0x1dda24c7, 0xee928277))

main :: IO ()
main = do
    putStrLn "Quarterround tests:"
    putStrLn $ if quarterround quarterroundInput1 == quarterroundOutput1 then "OK" else "FAIL!"
    putStrLn $ if quarterround quarterroundInput2 == quarterroundOutput2 then "OK" else "FAIL!"
    putStrLn $ if quarterround quarterroundInput3 == quarterroundOutput3 then "OK" else "FAIL!"
    putStrLn $ if quarterround quarterroundInput4 == quarterroundOutput4 then "OK" else "FAIL!"
    putStrLn $ if quarterround quarterroundInput5 == quarterroundOutput5 then "OK" else "FAIL!"
    putStrLn $ if quarterround quarterroundInput6 == quarterroundOutput6 then "OK" else "FAIL!"
    putStrLn ""

    putStrLn "Rowround tests:"
    putStrLn $ if rowround rowroundInput1 == rowroundOutput1 then "OK" else "FAIL!"
    putStrLn $ if rowround rowroundInput2 == rowroundOutput2 then "OK" else "FAIL!"
    putStrLn ""

    putStrLn "Columnround tests:"
    putStrLn $ if columnround columnroundInput1 == columnroundOutput1 then "OK" else "FAIL!"
    putStrLn $ if columnround columnroundInput2 == columnroundOutput2 then "OK" else "FAIL!"
    putStrLn ""

    putStrLn "Doubleround tests:"
    putStrLn $ if doubleround doubleroundInput1 == doubleroundOutput1 then "OK" else "FAIL!"
    putStrLn $ if doubleround doubleroundInput2 == doubleroundOutput2 then "OK" else "FAIL!"
    putStrLn ""


    return ()
