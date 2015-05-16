mumuki-hspec-server
===================

[![Build Status](https://travis-ci.org/mumuki/mumuki-hspec-server.svg)](https://travis-ci.org/mumuki/mumuki-hspec-server)

Snap server for running GHC tests, as part of [Mumuki](http://github.com/mumuki/mumuki) infrastructure

Please check the [Mumuki Wiki](https://github.com/mumuki/mumuki-platform/wiki) for more information.

# Building & running from source

## Initial setup

```
cabal sandbox init
cabal install --dependencies-only --enable-tests
cabal configure --enable-tests
```

## Building the server

```
cabal build
```

## Running tests

```
cabal test
```

## Running the server

```
cabal run
```

**Notice**: you may want to change the ```prod/config/Config``` file:

```haskell
--runhaskellArgs = ["-package-db=/app/.cabal-sandbox/x86_64-linux-ghc-7.8.3-packages.conf.d"]
runhaskellArgs = []
```
