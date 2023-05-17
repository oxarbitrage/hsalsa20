import Quarterround

import Data.Word


input1 :: (Word32, Word32, Word32, Word32)
input1 = (0, 0, 0, 0)

output1 :: (Word32, Word32, Word32, Word32)
output1 = (0, 0, 0, 0)

input2 :: (Word32, Word32, Word32, Word32)
input2 = (1, 0, 0, 0)

output2 :: (Word32, Word32, Word32, Word32)
output2 = (0x08008145, 0x00000080, 0x00010200, 0x20500000)

programOutput :: (Word32, Word32, Word32, Word32) -> (Word32, Word32, Word32, Word32)
programOutput input = (
    get0 (quarterround input), get1 (quarterround input), get2 (quarterround input), get3 (quarterround input))

main :: IO ()
main = do
  putStrLn ""
  putStrLn $ if programOutput input1 == output1 then "OK" else "FAIL!"
  putStrLn $ if programOutput input2 == output2 then "OK" else "FAIL!"

  return ()
