Building ProFeat from source
============================

Prerequisites
-------------

Download and install the [Stack](haskellstack.org) tool.

If you do not have a recent version of GHC on your system (at least version
8.0), run

    stack setup

in the project directory (containing the `stack.yaml` file) to install the
compiler.

Building ProFeat
----------------

To build ProFeat, run the following command in the project's root directory
(containing the `stack.yaml` file):

    stack build

Optionally, you may run

    stack install

to copy the `profeat` binary to ~/.local/bin.

