# Yielding Transaction Pattern SDK (ytxp-sdk)

This library includes YTxP types and instances for serialization, pretty
printing, and testing.

These types correspond to both `cardano-api` and `plutus-ledger-api`, making it
sufficient to support the development of YTxP-style protocols in various onchain
languages (such as `plutus-tx` and `plutarch`) and offchain frameworks (such as
`sc-tools` and `atlas`).

For more information on using the `ytxp-sdk`, please go to the
[`ytxp-lib` documentation](https://github.com/mlabs-haskell/ytxp-lib).

## Tooling

### Continuous Integration (CI)

The CI for this project runs using [Hercules CI](https://hercules-ci.com). All
the pre-commit checks will run in CI.

### Developer Experience (DevEx)

All the commands used for development purposes are exposed through the
[Makefile](./Makefile). To see the available commands, you can simply run:

```bash
make
```

### Formatting

The format of most of the source files is checked. You can use individual
commands through the `Makefile` or you can simply run:

```bash
make format_lint
```

to apply all the linters and formatters. This might be useful.

**Note:** Some linters cannot automatically fix your code. For example,
`markdownlint` may signal that a code block (delimited by ```) does not have the
language specified but cannot automatically infer the language of the code. This
means that in general, `make format_lint` does not resolve all the problems that
pre-commit checks can raise.

### Haddock Documentation

Below are the instructions for generating local Haddock documentation.

#### Build

##### Using Make Target

To build documentation directly, utilize the following make target:

```bash
make build_docs
```

After the execution, this command will specify the location of the generated
documentation.

## Tests

Tests will run in CI thanks to some specific checks in the Nix flake.

You can run tests:

- Using Nix: `nix flake check`: this will run all the checks, not only the
tests;
- Using Cabal directly (assuming it is present in the `$PATH`). See the
`Makefile` targets to check the available test suites.
