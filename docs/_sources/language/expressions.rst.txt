.. _sec-expressions:

Expressions
===========

Expressions can contain literal values, identifiers (constants, formulas,
local and global variables) and operators.  Literals for Booleans, integers and
doubles are written in the standard way: ``true``, ``false``, ``4``, ``3.14``.


Operators
---------

The following operators can be used in expressions, ordered by their precedence
(most strongly binding operators first):

* ``[...]``, ``.``, ``(...)`` (array indexing, member access, function call)
* ``-`` (unary minus)
* ``*``, ``/`` (multiplication, division)
* ``+``, ``-`` (addition, subtraction)
* ``>``, ``>=``, ``<``, ``<=`` (relational operators)
* ``=``, ``!=`` (equality operators)
* ``!`` (unary negation)
* ``&`` (logical and)
* ``|`` (logical or)
* ``=>`` (implication)
* ``... ? ... : ...`` (ternary if-then-else)


Built-in functions
------------------

The following built-in functions are provided:

* ``min(x, y, ...)`` and ``max(x, y, ...)`` which select the minimum or maximum
  of two or more numbers, respectively.
* ``floor(x)`` and ``ceil(x)`` which round ``x`` up and down, respectively, to
  the nearest integer.
* ``pow(x, y)`` which computes ``x`` to the power of ``y``.
* ``mod(i, n)`` for integer modulo.
* ``log(x, b)`` which computes the logarithm of ``x`` to base ``b``.
* ``active(f)`` returns ``true`` if the feature ``f`` is in the current feature
  combination.
* ``iactive(f)`` returns ``1`` if the feature ``f`` is in the current feature
  combination, and ``0`` otherwise.
