Global variables
================

In addition to local variables in modules, a ProFeat model can also contain
global variables. A global variables can have the same types as
:ref:`local variables <sec-module-definitions>`. Globals are defined using the
``global`` keyword::

   global x : [0 .. 2];
   global b : bool init true;
   global a : array [0 .. 2] of [0 .. 1] init 0;
