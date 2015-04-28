Building ProFeat from source
============================

Prerequisites
-------------

To build ProFeat from source you need the Glasgow Haskell Compiler (ghc) and
the cabal-install tool. These tools may be provided by the package repository
of your OS, alternatively see [www.haskell.org/downloads][haskell.org/downloads]
for downloads and instructions.

Building ProFeat
----------------

*   It is recommended to build the package in an isolated environment by
    creating a cabal sandbox (otherwise profeat and all its dependencies will
    be installed to your user package database). Make sure you create the
    sandbox in the directory that contains the `profeat.cabal` file:

        cabal sandbox init

*   To build ProFeat, execute the following command in the project's root
    directory (the directory containing the `profeat.cabal` file):

        cabal install

    The executable will be written to `profeat/.cabal-sandbox/bin/profeat`.

