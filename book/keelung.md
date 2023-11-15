# ZK-Salsa20 with Keelung

## Introduction

This article explores the Keelung experimental feature, demonstrating its application to convert the Salsa20 implementation into an R1CS system. This endeavor is driven by pure research and learning objectives, and its practical utility remains uncertain.

## Examples

In the next GHCi session, load the necessary modules and demonstrate the quarterroundKeelung function.

```
ghci> :load Quarterround
[1 of 3] Compiling Operators        ( Operators.hs, interpreted )
[2 of 3] Compiling Utils            ( Utils.hs, interpreted )
[3 of 3] Compiling Quarterround     ( Quarterround.hs, interpreted )
Ok, three modules loaded.
ghci> input = [1, 2, 3, 4]
ghci> quarterroundKeelung input
Keelung {
  Output:
    [ $U₃₂3
    , $U₃₂0
    , $U₃₂1
    , $U₃₂2
    ]


  Other side effects: 
    $U₃₂0 := 2 ⊕ RoL 7 (1 + 4)
    $U₃₂1 := 3 ⊕ RoL 9 ((2 ⊕ RoL 7 (1 + 4)) + 1)
    $U₃₂2 := 4 ⊕ RoL 13 ((3 ⊕ RoL 9 ((2 ⊕ RoL 7 (1 + 4)) + 1)) + (2 ⊕ RoL 7 (1 + 4)))
    $U₃₂3 := 1 ⊕ RoL 18 ((4 ⊕ RoL 13 ((3 ⊕ RoL 9 ((2 ⊕ RoL 7 (1 + 4)) + 1)) + (2 ⊕ RoL 7 (1 + 4)))) + (3 ⊕ RoL 9 ((2 ⊕ RoL 7 (1 + 4)) + 1)))
}
ghci> 
```

Compile the quarterroundKeelung function using the bn128 backend:

```
ghci> compile bn128 (quarterroundKeelung input)
Right R1CS {
  Constriant (2393): 
    Ordinary constraints (914):

      $135 + 21888242871839275222246405745257275088548364400416034343698204186575808495616$320 = 0
      1 + 21888242871839275222246405745257275088548364400416034343698204186575808495616$136 + 21888242871839275222246405745257275088548364400416034343698204186575808495616$321 = 0
      $137 + 21888242871839275222246405745257275088548364400416034343698204186575808495616$322 = 0
      $138 + 21888242871839275222246405745257275088548364400416034343698204186575808495616$323 = 0

...
432$1312 + 67108864$1313 + 134217728$1314 + 268435456$1315 + 536870912$1316 + 1073741824$1317 + 2147483648$1318 = 0

    Boolean constraints (1479):

      $0 = $0 * $0
        ...
      $1478 = $1478 * $1478

  Variables (1479):

    Output variables:       $0 ... $127
    Other variables:        $128 ... $1478

}
ghci>

```

Interpret the quarterroundKeelung function using gf181 and compare it with the compute function.

```
ghci> interpret gf181 (quarterroundKeelung input) [] []
[2552136791,642,329219,2702221316]
ghci> quarterroundCompute input
[2552136791,642,329219,2702221316]
ghci> 
```

Some of those equallity tests are done in the [keelung tests](../test/unit/keelung/Spec.hs). 

Finally, the keelung implementation of hsalsa20 is available from quarterround up to the hash module:

```
ghci> :load Hash
...
ghci> input = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
ghci> coreCompute input 10
[1404140658,617196034,2156026741,1598378282,1320572937,2911795930,848484521,1720253549,2259298508,1446763222,3297210283,2542453712,1082050453,992640523,3623268820,2227813485]
ghci> interpret gf181 (coreKeelung input 10) [] []
[1404140658,617196034,2156026741,1598378282,1320572937,2911795930,848484521,1720253549,2259298508,1446763222,3297210283,2542453712,1082050453,992640523,3623268820,2227813485]
ghci>
ghci> coreKeelung input 10
...
ghci> 
ghci> compile bn128 (coreKeelung input 10)
...
```

## Keelung commands

- [quarterroundKeelung](https://oxarbitrage.github.io/salsa20-docs/salsa20/Quarterround.html#v:quarterroundKeelung)
- [rowroundKeelung](https://oxarbitrage.github.io/salsa20-docs/salsa20/Rowround.html#v:rowroundKeelung)
- [columnroundKeelung](https://oxarbitrage.github.io/salsa20-docs/salsa20/Columnround.html#v:columnroundKeelung)
- [doubleroundKeelung](https://oxarbitrage.github.io/salsa20-docs/salsa20/Doubleround.html#v:doubleroundKeelung)
- [doubleroundRKeelung](https://oxarbitrage.github.io/salsa20-docs/salsa20/Doubleround.html#v:doubleroundRKeelung)
- [coreKeelung](https://oxarbitrage.github.io/salsa20-docs/salsa20/Hash.html#v:coreKeelung)
- [salsa20Keelung](https://oxarbitrage.github.io/salsa20-docs/salsa20/Hash.html#v:salsa20Keelung)
