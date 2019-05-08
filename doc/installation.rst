Installation
============

ProFeat runs on Windows, Linux and MacOS. This section describes how ProFeat can
be built from a source code distribution.


Prerequisites
-------------

For building ProFeat from source, the tool ``stack`` must be installed. For
installation instructions, visit the
`stack website <https://www.haskellstack.org>`_.


Building from source
--------------------

In the projects root directory, which contains the ``profeat.cabal`` file, run
the following command to build the tool::

   stack build

The tools needed for building and all required dependencies will be downloaded
automatically.

Optionally, the ``profeat`` binary can be copied to the local bin path by using
the command::

   stack install
