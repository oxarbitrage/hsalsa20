# Haskell salsa20 cipher

Compute and Analyze Salsa20 Expressions from an Interactive Haskell Terminal.

Welcome to the Haskell Salsa20 Cipher project! This project serves as both a practical implementation and a learning playground for the Salsa20 algorithm, written in Haskell.

## Project Overview

This codebase is primarily a personal project, initiated for the purpose of learning Haskell and functional programming. While it's evolved and some features have stabilized, it remains an experimental and educational endeavor. 

**Tested on Linux and macOS (Untested on Windows)**

## Features

- Salsa20 Encryption and Decryption Demo: Explore the Salsa20 encryption and decryption application by running the `stack run` or `cabal run salsa20-exe` commands.

- Extensive Testing: The project includes a comprehensive suite of tests to ensure code quality. You can run the tests locally using `stack test` or `cabal test`. The CI (Continuous Integration) pipeline also runs most of these tests.

- Every cipher function is refined using [LiquidHaskell](https://github.com/ucsd-progsys/liquidhaskell) formal verification tool.

## Tutorial

For a detailed guide on using this Salsa20 cipher, check out our [tutorial](book/tutorial.md). The tutorial breaks down the Salsa20 algorithm into its core components, explains the underlying concepts, and provides practical examples.

## Application

You can run the demo application with `stack run` command, for example:

```bash
---Salsa20 encryption and decryption

Insert your secret key phrase:
secret key here
Insert message to be encrypted or decrypted:
testing some sort of message

Plain text: testing some sort of message
Secret key: secret key here
Key used: Tix73W8AQv7OKbGbxr7W7d3ZqR9YwcHYazNoUXrP0vo=

---Salsa20 Encrypt
Nonce: Gww7O2hiZ2Q=
Ciphertext: xwlqEAOSot6wzT37HuBFLhJiacLSC90yobCoTA==

---Salsa20 Decrypt
Decrypted: testing some sort of message
```

Please refer to the [application source code](https://github.com/oxarbitrage/hsalsa20/blob/main/app/Main.hs) for more information.

## API Documentation

Explore the API documentation for a deeper understanding of the project. Visit [here](https://oxarbitrage.github.io/salsa20-docs/) for detailed information on the project's functions and modules.

## Formal verification

Read more about formal verification process we are using in this library in the [formal verification page](book/formal-verification.md).

## Experimental: Keelung Code

One experimental aspect of this project is the Keelung code. The current plan involves compiling the core function into an `r1cs` circuit using Keelung for educational purposes. This experimental feature is designed to explore new possibilities and enhance learning. Read the [keelung chapter from the book](book/keelung.md) for examples.

## Get Involved

We welcome contributions, feedback, and discussions! If you have ideas, find issues, or want to contribute to the project, please feel free to open issues or submit pull requests. Your input and contributions are highly valued.

## License

This project is open-source and released under the [MIT](LICENSE-MIT) and [APACHE](LICENCE-APACHE) licenses.

---

**Happy ciphering with Haskell Salsa20!**
