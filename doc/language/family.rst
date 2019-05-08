.. _sec-family:

Family definition
=================

A family of systems is implicitly defined by the set of allowed (or valid)
:ref:`feature combinations <sec-feature-model>`. However, a family can also
be defined using an explicit family definition::

   family
       n : [0..3];
       b : bool;
       initial constraint b => n != 1;
   endfamily

A ``family`` block contains one or more *parameters* and optionally some
initial constraints on these parameters. The difference between parameters and
feature attributes is that parameters are assumed to be constant. As such, they
can be used anywhere in the model, including array sizes, number of
multi-feature instances and loop bounds. This means a family definition can be
used to parametrize the structure of a system.

If a ``family`` block is present and the ``--one-by-one`` :ref:`option
<sec-options>` is used, the generated instances are solely defined by the
parameters in the ``family`` block. This means that the set of initial states
(of each instance) is the set of valid configurations of the feature model.
However, features can also be used as family parameters by referencing them in
the ``family`` block::

    family
        n : [1..2];
        features my_feat;
    endfamily

    root feature
        [0..1] of my_feat;
    endfeature

    feature my_feat endfeature

Here, the feature ``my_feat`` is used as a family parameter. Thus, the family
shown above has 4 members.
