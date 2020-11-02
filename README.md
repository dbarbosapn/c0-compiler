## How to test

- First, configure cabal to use the test suite `cabal configure --enable-tests`
- Then, all you gotta do is run `cabal test`

## Versão do Énio

## Run test
cabal new-test --enable-tests
## Or
cabal new-configure --enable-tests
cabal new-test

## Run exe
cabal new-run --disable-tests
## Or
cabal new-configure --disable-tests
cabal new-run


