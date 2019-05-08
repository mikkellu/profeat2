Formulas
========

Similar to PRISM, ProFeat supports the definition of formulas to reduce code
duplication. In its simplest form, a formula definition provides a name for
an expression and is defined using the ``formula`` keyword::

   formula all_work = Consumer[0].work + Consumer[1].work;

The name introduced by the formula definition can then be used anywhere in the
model.

ProFeat also allows the parametrization of formulas, which effectively allows us
to define functions::

   formula fact(n) = for i in [1..n] i * ... endfor;

A parametrized formula can then be used like any built-in function,
for instance::

   const int n = fact(5);

.. note:: A formula definition may contain other formulas. However, they must
   not be recursive or mutually recursive.
