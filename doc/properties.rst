Property specification
======================

Properties are defined using PRISM's property specification language.
For details, we refer to the
`PRISM manual <http://www.prismmodelchecker.org/manual/PropertySpecification/Introduction>`_.
ProFeat provides a syntactic construct for embedding ProFeat
:ref:`expressions <sec-expressions>` in properties. All expressions between
``${`` and ``}`` are interpreted as ProFeat expressions. Consider the following
example::

   P=? [ F ${ Consumer[0].work = 0 & Buffer.cell[0] = -1 } ];

Here, a ProFeat expression is used to define the set of states for the ``F``
operator.

.. note:: The ``${ ... }`` construct cannot be nested.

All expressions marked with ``${ ... }`` are translated into a PRISM expression
when translating a properties file using ProFeat.
