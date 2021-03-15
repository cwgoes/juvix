# Juvix
<img src="Juvix_logo.png" width="250" height="250">

![GitHub](https://img.shields.io/github/license/metastatedev/juvix)
![Build status](https://img.shields.io/circleci/build/github/metastatedev/juvix?token=abc123def456)
![GitHub issues](https://img.shields.io/github/issues/metastatedev/juvix)

## Overview

[Juvix](https://juvix.org/) synthesizes a high-level frontend syntax, dependent-linearly-typed core language, whole-program optimisation system,
and backend-swappable execution model into a single unified stack for writing formally verifiable, efficiently executable
smart contracts which can be deployed to a variety of distributed ledgers.

Juvix is designed to address the problems that we have experienced while trying to write & deploy decentralised applications and that we observe in the ecosystem at large:
the difficulty of effective verification, the ceiling of compositional complexity, the illegibility of execution costs, and the lock-in to particular backends.
In order to do so, Juvix draws upon and aims to productionise a deep reservoir of prior academic research in programming language design & type theory which we believe has a high degree of applicability to these problems.

Juvix's compiler architecture is purpose-built from the ground up for the particular requirements and economic trade-offs
of the smart contract use case — it prioritises behavioural verifiability, semantic precision, and output code efficiency over compilation speed,
syntactical familiarity, and backwards compatibility with existing blockchain virtual machines.

For more design details, see [the language
reference](./doc/reference/language-reference.pdf).

<!--## Screenshots TODO--> 
<!-- Include logo/demo screenshot with labels. 
- vscode screenshot
- eg contracts -->

## Caveats

This is pre-alpha software released for experimentation & research purposes only.

Do not expect API stability. Expect bugs. Expect divergence from canonical protocol implementations.

Formal verification of various properties of the Juvix language & compiler in Agda is [in progress](experimental/qtt-agda) but not yet complete.

No warranty is provided or implied.

## Installation

<!-- ### Requirements

The following are required to build Juvix:

- [libff](https://github.com/scipr-lab/libff)
- [libsecp256k1](https://github.com/bitcoin-core/secp256k1)
- [Openssl Libssl API](https://wiki.openssl.org/index.php/Libssl_API) -->


[Stack](https://haskellstack.org) required. 8GB RAM required for `stack` installation.

  - For Ubuntu        : `apt install stack`
  - For Debian        : `apt install haskell-stack`
  - For Arch Linux    : `pacman -S stack`
  - For macOS : `brew install haskell-stack`
  - For Windows, following the instructions [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows).
<!-- - **libsecp256k1**
  - For Ubuntu/Debian : `apt install libsecp256k1-dev`
  - For Arch Linux : `pacman -S libsecp256k1`
  - For macOS : `brew tap cuber/homebrew-libsecp256k1 && brew install libsecp256k1`
- **Openssl Libssl API**
  - For Ubuntu/Debian : `apt install libssl-dev`
  - For Arch Linux : `pacman -S openssl`
  - For macOS : `brew install openssl` -->

### Building

After cloning Juvix into a local directory, go into the local Juvix directory, and build and install the binary to the local path with:

```bash
make
```
Expect the installation to take some time.  
For Windows users: to be able to use the command *make*, please visit [this link](https://stackoverflow.com/questions/32127524/how-to-install-and-use-make-in-windows).

### Building with optimisations

For full optimisations (but slower compile times):

```bash
make build-prod
```

## Usage

### Writing and compiling your first `.ju` contract

See the tutorials and documentations on the [Juvix website](https://juvix.org/).

### Visual Studio Code support

Install the [Juvix package](https://marketplace.visualstudio.com/items?itemName=metastate.language-juvix)
to get syntax highlighting support for Juvix in VSCode.

Other IDE supports will be added over time.

## Report a bug

If you found a bug please open an issue and detail the bug. We will fix it as soon as we can.

## Contributing

We welcome contributions to the development of Juvix. See
[CONTRIBUTING.md](./doc/CONTRIBUTING.md) for contribution guidelines.

### Installation requirements

#### Formatter

[Ormolu](https://github.com/tweag/ormolu) required for source formatting. Run
`stack install ormolu` to get the latest version (0.0.3.1).

#### Documentation Generator

[Roswell](https://github.com/roswell/roswell) is required for automatic generation of documentation in [doc/Code](https://github.com/metastatedev/juvix/tree/develop/doc/Code).

Once Roswell is installed one only needs to add `~/.roswell/bin` to their bash path along with running `ros install heliaxdev/org-generation`.

Then run `scripts/precommit.sh`.

#### REPL

To open a REPL with the library scoped:

```bash
make repl-lib
```

To open a REPL with the executable scoped:

```bash
make repl-exe
```

## Be part of the community

We would love to hear what you think of Juvix! Join our community:

- Follow us on [Twitter](https://twitter.com/juvixlang)
- Subscribe to our [newsletter](https://juvix.org/)

<!-- TODO add links to discord, reddit, etc. -->

