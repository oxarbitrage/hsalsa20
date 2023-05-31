import Quarterround
import Rowround
import Columnround
import Doubleround
import Salsa20
import Expansion
import Utils
import Types

import Data.Word
import Data.Tuple.Select
import Text.Printf

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

-- Littleendian

littleendianInput1 :: VectorType
littleendianInput1 = (0, 0, 0, 0)

littleendianOutput1 :: Word32
littleendianOutput1 = 0

littleendianInput2 :: VectorType
littleendianInput2 = (86, 75, 30, 9)

littleendianOutput2 :: Word32
littleendianOutput2 = 0x091e4b56

littleendianInput3 :: VectorType
littleendianInput3 = (255, 255, 255, 250)

littleendianOutput3 :: Word32
littleendianOutput3 = 0xfaffffff

-- Salsa20

salsa20Input1 :: Matrix64Type
salsa20Input1 = (
    ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0)),
    ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0)),
    ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0)), 
    ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0)))

salsa20Output1 :: Matrix64Type
salsa20Output1 = (
    ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0)), 
    ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0)),
    ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0)), 
    ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0)))

salsa20Input2 :: Matrix64Type
salsa20Input2 = (
    ((211, 159, 13,115), (76, 55, 82, 183), (3, 117, 222, 37), (191, 187, 234, 136)),
    ((49, 237, 179, 48), (1, 106, 178, 219), (175, 199, 166, 48), (86, 16, 179, 207)),
    ((31, 240, 32, 63), (15, 83, 93, 161), (116, 147, 48, 113), (238, 55, 204, 36)), 
    ((79, 201, 235, 79), (3, 81, 156, 47), (203, 26, 244, 243), (88, 118, 104, 54)))

salsa20Output2 :: Matrix64Type
salsa20Output2 = (
    ((109, 42, 178, 168), (156, 240, 248, 238), (168, 196, 190, 203), (26, 110, 170, 154)), 
    ((29, 29, 150, 26), (150, 30, 235, 249), (190, 163, 251, 48), (69, 144, 51, 57)),
    ((118, 40, 152, 157), (180, 57, 27, 94), (107, 42, 236, 35), (27, 111, 114, 114)), 
    ((219, 236, 232, 135), (111, 155, 110, 18), (24, 232, 95, 158), (179, 19, 48, 202)))

salsa20Input3 :: Matrix64Type
salsa20Input3 = (
    ((88, 118, 104, 54), (79, 201, 235, 79), (3, 81, 156, 47), (203, 26, 244, 243)), 
    ((191, 187, 234, 136), (211, 159, 13, 115), (76, 55, 82, 183), (3, 117, 222, 37)),
    ((86, 16, 179, 207), (49, 237, 179, 48), (1, 106, 178, 219), (175, 199, 166, 48)), 
    ((238, 55, 204, 36), (31, 240, 32, 63), (15, 83, 93, 161), (116, 147, 48, 113)))

salsa20Output3 :: Matrix64Type
salsa20Output3 = (
    ((179, 19, 48, 202), (219, 236, 232, 135), (111, 155, 110, 18), (24, 232, 95, 158)), 
    ((26, 110, 170, 154), (109, 42, 178, 168), (156, 240, 248, 238), (168, 196, 190, 203)),
    ((69, 144, 51, 57), (29, 29, 150, 26), (150, 30, 235, 249), (190, 163, 251, 48)), 
    ((27, 111, 114, 114), (118, 40, 152, 157), (180, 57, 27, 94), (107, 42, 236, 35)))

-- Expanded Salsa20

expandedk0 :: MatrixType
expandedk0 = ((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12), (13, 14, 15, 16))

expandedk1 :: MatrixType
expandedk1 = ((201, 202, 203, 204), (205, 206, 207, 208), (209, 210, 211, 212), (213, 214, 215, 216))

expandedn :: MatrixType
expandedn = ((101, 102, 103, 104), (105, 106, 107, 108), (109, 110, 111, 112), (113, 114, 115, 116))

expandedOutput1 :: Matrix64Type
expandedOutput1 = (
    ((69, 37, 68, 39), (41, 15, 107, 193), (255, 139, 122, 6), (170, 233, 217, 98)),
    ((89, 144, 182, 106), (21, 51, 200, 65), (239, 49, 222, 34), (215, 114, 40, 126)),
    ((104, 197, 7, 225), (197, 153, 31, 2), (102, 78, 76, 176), (84, 245, 246, 184)),
    ((177, 160, 133, 130), (6, 72, 149, 119),(192, 195, 132, 236), (234, 103, 246, 74)))

expandedOutput2 :: Matrix64Type
expandedOutput2 = (
    ((39, 173, 46, 248), (30, 200, 82, 17), (48, 67, 254, 239), (37, 18, 13, 247)),
    ((241, 200, 61, 144), (10, 55, 50, 185), (6, 47, 246, 253), (143, 86, 187, 225)),
    ((134, 85, 110, 246), (161, 163, 43, 235), (231, 94, 171, 51), (145, 214, 112, 29)),
    ((14, 232, 5, 16), (151, 140, 183, 141), (171, 9, 122, 181), (104, 182, 177, 193)))

main :: IO ()
main = do
    putStrLn "Quarterround tests:"
    putStrLn $ if quarterroundCompute quarterroundInput1 == quarterroundOutput1 then "OK" else "FAIL!"
    putStrLn $ if quarterroundCompute quarterroundInput2 == quarterroundOutput2 then "OK" else "FAIL!"
    putStrLn $ if quarterroundCompute quarterroundInput3 == quarterroundOutput3 then "OK" else "FAIL!"
    putStrLn $ if quarterroundCompute quarterroundInput4 == quarterroundOutput4 then "OK" else "FAIL!"
    putStrLn $ if quarterroundCompute quarterroundInput5 == quarterroundOutput5 then "OK" else "FAIL!"
    putStrLn $ if quarterroundCompute quarterroundInput6 == quarterroundOutput6 then "OK" else "FAIL!"
    putStrLn ""

    putStrLn "Print a quarterround expression test:"
    putStrLn $ printf "z1 = %s" (sel2 (quarterroundDisplay quarterroundInput6))
    putStrLn $ printf "z2 = %s" (sel3 (quarterroundDisplay quarterroundInput6))
    putStrLn $ printf "z3 = %s" (sel4 (quarterroundDisplay quarterroundInput6))
    putStrLn $ printf "z0 = %s" (sel1 (quarterroundDisplay quarterroundInput6))

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

    putStrLn "Littleendian tests:"
    putStrLn $ if littleendian littleendianInput1 == littleendianOutput1 then "OK" else "FAIL!"
    putStrLn $ if littleendian littleendianInput2 == littleendianOutput2 then "OK" else "FAIL!"
    putStrLn $ if littleendian littleendianInput3 == littleendianOutput3 then "OK" else "FAIL!"
    putStrLn ""

    putStrLn "Littleendian inverse tests:"
    putStrLn $ if littleendianInv littleendianOutput1 == littleendianInput1 then "OK" else "FAIL!"
    putStrLn $ if littleendianInv littleendianOutput2 == littleendianInput2 then "OK" else "FAIL!"
    putStrLn $ if littleendianInv littleendianOutput3 == littleendianInput3 then "OK" else "FAIL!"
    putStrLn ""

    putStrLn "Salsa20 tests:"
    putStrLn $ if salsa20 salsa20Input1 == salsa20Output1 then "OK" else "FAIL!"
    putStrLn $ if salsa20 salsa20Input2 == salsa20Output2 then "OK" else "FAIL!"
    putStrLn $ if salsa20 salsa20Input3 == salsa20Output3 then "OK" else "FAIL!"
    putStrLn ""

    putStrLn "Expanded Salsa20 tests:"
    putStrLn $ if expand32 expandedk0 expandedk1 expandedn == expandedOutput1 then "OK" else "FAIL!"
    putStrLn $ if expand16 expandedk0 expandedn == expandedOutput2 then "OK" else "FAIL!"
    putStrLn ""

    return ()