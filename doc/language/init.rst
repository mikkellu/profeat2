.. _sec-init:

Multiple initial states
=======================

If a variable is not initialized, its initial value will be chosen
nondeterministically. Then, each instance of a system family has multiple
initial states. To constrain the set of initial states, an ``init`` block can
be specified. The ``init`` block contains a Boolean expression over the
variables in the model. Only those states for which this expression evaluates
to ``true`` are initial states. Consider the following example::


   root feature
       modules my_module;
   endfeature

   module my_module
       x : bool;
       y : [-1 .. 1];
   endmodule

   init
       x => y = 0
   endinit

Since both ``x`` and ``y`` are left uninitialized, their value is chosen
nondeterministically. However, if ``x = true`` then ``y = 0`` because all
initial states must satisfy the expression given in the ``init`` block.
