{-|
https://github.com/das-labor/legacy/blob/master/microcontroller-2/arm-crypto-lib/testvectors/salsa20-256.64-verified.test-vectors
-}
module Vectors
(
    set1_vector0_key1,
    set1_vector0_key2,
    set1_vector0_iv,
    set1_vector0_expected_output_0_63,
    set1_vector0_expected_output_192_255,
    set1_vector0_expected_output_256_319,
    set1_vector0_expected_output_448_511,

    set1_vector9_key1,
    set1_vector9_key2,
    set1_vector9_iv,
    set1_vector9_expected_output_0_63,
    set1_vector9_expected_output_192_255,
    set1_vector9_expected_output_256_319,
    set1_vector9_expected_output_448_511,

    set1_vector18_key1,
    set1_vector18_key2,
    set1_vector18_iv,
    set1_vector18_expected_output_0_63,
    set1_vector18_expected_output_192_255,
    set1_vector18_expected_output_256_319,
    set1_vector18_expected_output_448_511,

    set1_vector27_key1,
    set1_vector27_key2,
    set1_vector27_iv,
    set1_vector27_expected_output_0_63,
    set1_vector27_expected_output_192_255,
    set1_vector27_expected_output_256_319,
    set1_vector27_expected_output_448_511,

    set1_vector36_key1,
    set1_vector36_key2,
    set1_vector36_iv,
    set1_vector36_expected_output_0_63,
    set1_vector36_expected_output_192_255,
    set1_vector36_expected_output_256_319,
    set1_vector36_expected_output_448_511,
) where

-- Set 1, vector#  0:

set1_vector0_key1 :: String
set1_vector0_key1 = "80000000000000000000000000000000"

set1_vector0_key2 :: String
set1_vector0_key2 = "00000000000000000000000000000000"

set1_vector0_iv :: String
set1_vector0_iv = "0000000000000000"

set1_vector0_expected_output_0_63 :: String
set1_vector0_expected_output_0_63 = "E3BE8FDD8BECA2E3EA8EF9475B29A6E7\
    \003951E1097A5C38D23B7A5FAD9F6844\
    \B22C97559E2723C7CBBD3FE4FC8D9A07\
    \44652A83E72A9C461876AF4D7EF1A117"

set1_vector0_expected_output_192_255 :: String
set1_vector0_expected_output_192_255 = "57BE81F47B17D9AE7C4FF15429A73E10\
    \ACF250ED3A90A93C711308A74C6216A9\
    \ED84CD126DA7F28E8ABF8BB63517E1CA\
    \98E712F4FB2E1A6AED9FDC73291FAA17"

set1_vector0_expected_output_256_319 :: String
set1_vector0_expected_output_256_319 = "958211C4BA2EBD5838C635EDB81F513A\
    \91A294E194F1C039AEEC657DCE40AA7E\
    \7C0AF57CACEFA40C9F14B71A4B3456A6\
    \3E162EC7D8D10B8FFB1810D71001B618"

set1_vector0_expected_output_448_511 :: String
set1_vector0_expected_output_448_511 = "696AFCFD0CDDCC83C7E77F11A649D79A\
    \CDC3354E9635FF137E929933A0BD6F53\
    \77EFA105A3A4266B7C0D089D08F1E855\
    \CC32B15B93784A36E56A76CC64BC8477"

-- Set 1, vector#  9:

set1_vector9_key1 :: String
set1_vector9_key1 = "00400000000000000000000000000000"

set1_vector9_key2 :: String
set1_vector9_key2 = "00000000000000000000000000000000"

set1_vector9_iv :: String
set1_vector9_iv = "0000000000000000"

set1_vector9_expected_output_0_63 :: String
set1_vector9_expected_output_0_63 = "01F191C3A1F2CC6EBED78095A05E062E\
    \1228154AF6BAE80A0E1A61DF2AE15FBC\
    \C37286440F66780761413F23B0C2C9E4\
    \678C628C5E7FB48C6EC1D82D47117D9F"

set1_vector9_expected_output_192_255 :: String
set1_vector9_expected_output_192_255 = "86D6F824D58012A14A19858CFE137D76\
    \8E77597B96A4285D6B65D88A7F1A8778\
    \4BF1A3E44FC9D3525DDC784F5D99BA22\
    \2712420181CABAB00C4B91AAEDFF521C"

set1_vector9_expected_output_256_319 :: String
set1_vector9_expected_output_256_319 = "287A9DB3C4EEDCC96055251B73ED361B\
    \A727C2F326EF6944F9449FB7A3DDC396\
    \A88D9D0D853FADE365F82789D57F9B40\
    \10F963BC498F176A93FD51723FCD4D55"

set1_vector9_expected_output_448_511 :: String
set1_vector9_expected_output_448_511 = "E0D62E2E3B37FDD906C934FAA35D5E8A\
    \89A517DD0F24CF33DE8495C5FF24F4B1\
    \476B3E826A1C90D74507C3991CEF4067\
    \E316A04B97AEFFA5E9D1F33CB0609B9E"

-- Set 1, vector#  18:

set1_vector18_key1 :: String
set1_vector18_key1 = "00002000000000000000000000000000"

set1_vector18_key2 :: String
set1_vector18_key2 = "00000000000000000000000000000000"

set1_vector18_iv :: String
set1_vector18_iv = "0000000000000000"

set1_vector18_expected_output_0_63 :: String
set1_vector18_expected_output_0_63 = "C29BA0DA9EBEBFACDEBBDD1D16E5F598\
    \7E1CB12E9083D437EAAAA4BA0CDC909E\
    \53D052AC387D86ACDA8D956BA9E6F654\
    \3065F6912A7DF710B4B57F27809BAFE3"

set1_vector18_expected_output_192_255 :: String
set1_vector18_expected_output_192_255 = "77DE29C19136852CC5DF78B5903CAC7B\
    \8C91345350CF97529D90F18055ECB75A\
    \C86A922B2BD3BD1DE3E2FB6DF9153166\
    \09BDBAB298B37EA0C5ECD917788E2216"

set1_vector18_expected_output_256_319 :: String
set1_vector18_expected_output_256_319 = "1985A31AA8484383B885418C78210D0E\
    \84CBC7070A2ED22DCAAC6A739EAD5881\
    \8E5F7755BE3BF0723A27DC69612F18DC\
    \8BF9709077D22B78A365CE6131744651"

set1_vector18_expected_output_448_511 :: String
set1_vector18_expected_output_448_511 = "9618FCA736A8ECA00BD1194FC9855085\
    \526ECD47A8DE1F8DB298AD49FCE935EA\
    \63B548597092ABAD6338F41AF87586A7\
    \0505F2537902B81F55E53599DABA84CC"

-- Set 1, vector#  27:

set1_vector27_key1 :: String
set1_vector27_key1 = "00000010000000000000000000000000"

set1_vector27_key2 :: String
set1_vector27_key2 = "00000000000000000000000000000000"

set1_vector27_iv :: String
set1_vector27_iv = "0000000000000000"

set1_vector27_expected_output_0_63 :: String
set1_vector27_expected_output_0_63 = "FF852567EB72687DC56C122D61B2FB2A\
    \4FB9E8E8DA62313B618D10F8E0DA521B\
    \176E879CD78E641043F0FA4A22211566\
    \429B7C68EC645FF5E44B2505D61A2D71"

set1_vector27_expected_output_192_255 :: String
set1_vector27_expected_output_192_255 = "E5B040B199C3DFC8DB1F41C74C798AE2\
    \62105477AEB1CE761D6FFF1CAB15AA1A\
    \7B7CE26B9CCE6DC33FD4522BF8F73E70\
    \B843D67FC06FA2258F9709DB14FBD54C"

set1_vector27_expected_output_256_319 :: String
set1_vector27_expected_output_256_319 = "55706075E5FED81E2205994609868EFC\
    \383B3E4CC295C4214356BA41FC72BFE5\
    \4E6936FE6684EAF93C5973DDCD8E8F23\
    \767B82D783953F89AF4E808C90BEEABD"

set1_vector27_expected_output_448_511 :: String
set1_vector27_expected_output_448_511 = "7ECE71883742EE852C94F01AD85EA1A6\
    \76CC7CBC6EDFCF1BAE751455A923FAAC\
    \806BB72E6A982EC7A38F112445E25EB6\
    \BC5B49C5E6C22DC8748DEE0942F6E8B2"

-- Set 1, vector#  36:

set1_vector36_key1 :: String
set1_vector36_key1 = "00000000080000000000000000000000"

set1_vector36_key2 :: String
set1_vector36_key2 = "00000000000000000000000000000000"

set1_vector36_iv :: String
set1_vector36_iv = "0000000000000000"

set1_vector36_expected_output_0_63 :: String
set1_vector36_expected_output_0_63 = "AF6E2EE1D5021675A92F02C764AFD94A\
    \F3097F53532FC965EB861D6D12A3A012\
    \ABA683A5281238CE76E3AF3944736752\
    \AD86A5FD16E7DAFAF241ECFB0ADBBDFE"

set1_vector36_expected_output_192_255 :: String
set1_vector36_expected_output_192_255 = "19444E6D7C3D8BEC0957C3E785E1EEFD\
    \56B857F21CF8D325A4285F8DEF5078FF\
    \7B7EFB5E3B20F6E0906265B6F7580A04\
    \9CEC5DF1872DCCB54081054C0FC15514"

set1_vector36_expected_output_256_319 :: String
set1_vector36_expected_output_256_319 = "7EB544ADBF57D042E3A6753B13C65843\
    \0399764CF90D007E48DAFE3DA1FE3F90\
    \8EF4BFA6AF96DCD54197DA0D3A10FA35\
    \6A374DA08B9A84044E70EC70ED050D46"

set1_vector36_expected_output_448_511 :: String
set1_vector36_expected_output_448_511 = "57224DA912C62801DB393D5E3F4EDFF7\
    \D61BA895F88C7391FE5C943B88CC4642\
    \0D11C3F1884B628F03C04A3C10F03FFB\
    \CFC652D066BFD8DBF52DA2A72B9B9AC5"
