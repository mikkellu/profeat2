The ProFeat language
====================

ProFeat provides a modeling language for describing families of stochastic
systems. The ProFeat language extends the input language of the probabilistic
model checker `PRISM <http://www.prismmodelchecker.org>`_ by feature-related
concepts and meta-programming constructs.

A ProFeat model consists of two parts. The first part, the
:ref:`feature model <sec-feature-model>`,
defines a set of features and specifies the allowed feature combinations. The
second part describes the behavior of the individual features using
:ref:`feature modules <sec-modules>`.
It optionally includes a :ref:`feature controller <sec-controller>`, which
specifies the dynamic feature switches.

.. toctree::
   :maxdepth: 1
   :caption: Contents:

   language/features
   language/family
   language/modules
   language/controller
   language/constants
   language/globals
   language/formulas
   language/expressions
   language/rewards
   language/labels
   language/init
   language/invariants
   language/blocks
