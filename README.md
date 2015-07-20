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

**Notice**: you may want to change the following files:

* ```prod/config/Config``` (for running the runner locally)
* ```prod/config/Test```  (for testing the runner locally)

with local configuration

```haskell
runhaskellArgs = ["-package-db=./.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d"]
```
