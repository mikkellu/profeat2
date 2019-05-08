Invariants
==========

Similar to an ``init`` block, which constrains the initial states of a model, an
``invariant`` block constrains the set of reachable states (this includes the
initial states). Consider the following example::

   invariant
       mod(my_feature.x, 2) = 0;
   endinvariant

   feature my_feature
       modules my_module;
   endfeature

   module my_module
       x : [0..4];

       [] x < 4 -> (x' = x + 1);
       [] x < 3 -> (x' = x + 2);
   endmodule

In this example, the first command of ``my_module`` will never be executed, since
adding 1 to ``x`` would violate the invariant. However, the second command is
not affected, as it does not lead to a violation of the invariant.
