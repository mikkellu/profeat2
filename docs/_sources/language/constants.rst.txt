Constants
=========

ProFeat allows the definition of *constants*. A constant can be defined in terms
of literals or expressions including other constants. They are defined using the
``const`` keyword::

   const int x = 5;
   const bool b = true;
   const double p = 0.5;

Constant arrays can be defined by setting the constant to an array literal::

   const int values = {1, 2, 3};

If no type is given after the ``const`` keyword, the ``int`` type is assumed.
