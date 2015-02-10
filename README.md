Building ProFeat from source
============================

Prerequisites
-------------

To build ProFeat from source you will at least need GHC and cabal. These are
provided by the [Haskell Platform](haskell.org/platform).

Building ProFeat
----------------

*   It is recommended to build the package in an isolated environment by
    creating a cabal sandbox (otherwise profeat and all its dependencies will
    be installed to your user package database). Make sure you create the
    sandbox in the directory that contains the `profeat.cabal` file:

        cd profeat/
        cabal sandbox init
        cabal sandbox add-source ../sobdd

    If you decide not to use a sandbox, you have to install the `sobdd` library
    manually to your user package database:

        cd sobdd/
        cabal install

*   To build ProFeat, execute the following command in the project's root
    directory (the directory containing the `profeat.cabal` file):

        cabal install

    The executable will be written to `profeat/.cabal-sandbox/bin/profeat`.

