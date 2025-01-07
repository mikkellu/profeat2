## Note

This is a fork of the original ProFeat tool (version 1.0.3.0). This fork adds support for [uncertain models](https://www.prismmodelchecker.org/manual/ThePRISMLanguage/UncertainModels), i.e., models where the transition probabilities are not known exactly, but are given as interval probabilities.  
The original ProFeat tool can be found [here](https://github.com/pchrszon/profeat/tree/master).

# ProFeat

ProFeat is a tool for the analysis of stochastic system families.
It provides a modeling language that extends the input language of the
probabilistic model checker [PRISM](http://www.prismmodelchecker.org) by
feature-oriented concepts, including support for dynamic feature switches,
multi-features and feature attributes.

## Documentation

The feature-oriented modeling language and the usage of the tool are described
in the [documentation](https://pchrszon.github.io/profeat).

## Installation

### Prerequisites

For building ProFeat from source, the tool `stack` must be installed. For
installation instructions, visit the
[stack website](https://www.haskellstack.org).

To generate the user documentation, [Sphinx](https://www.sphinx-doc.org) must be
installed.

### Building from source

In the projects root directory, which contains the `profeat.cabal` file, run
the following command to build the tool:

    stack build

The tools needed for building and all required dependencies will be downloaded
automatically.

Optionally, the `profeat` binary can be copied to the local bin path by using
the command:

    stack install

### Generating the documentation

To build the user documentation, go to the `doc` directory and run the following
command:

    make html

For other documentation formats, consult the Makefile or the Sphinx
documentation.
