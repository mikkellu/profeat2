.. _sec-feature-model:

Feature model
=============

The feature model defines a set of features and the allowed (or valid)
combinations of these features. A ProFeat feature model follows the usual
hierarchical structure and may contain additional cross-tree constraints.


Feature definitions
-------------------

A *feature* is introduced by a ``feature`` block, as shown in the following
example::

   feature Worker

   endfeature

The *root feature*, i.e., the root of the feature model tree, is defined using
the ``root feature`` block::

   root feature

   endfeature

.. note:: The root feature definition does not include a name.


Decomposition
-------------

A feature may have one or more child features (or subfeatures), i.e., the
feature is decomposed into subfeatures. A decomposition defines the constraints
between a parent feature and its subfeatures. In the following example, the
root feature is decomposed into the subfeatures ``Producer``, ``Consumer`` and
``Buffer``::

   root feature
       all of Producer, Consumer, Buffer;
   endfeature

   feature Producer endfeature
   feature Consumer endfeature
   feature Buffer endfeature

.. note:: Each feature referenced in a feature decomposition must be defined
   using a feature block. A feature may by referenced by multiple other
   features. In this case multiple instances of the feature are created. This
   means that a feature which is referenced multiple times is not shared, but
   copied.

The ``all of`` decomposition states that all of the referenced features must
be part of the feature combination in case the parent feature is part of the
feature combination. In the above example, the root feature is the parent
feature. Since the root feature is contained in every feature combination, so
are the ``Producer``, ``Consumer`` and ``Buffer`` features.

The following table lists all decomposition operators.

===================  ======================================================================
Decomposition        If the parent feature is contained in the feature combination, then...
===================  ======================================================================
``all of``           all subfeatures must be contained.
``one of``           exactly one subfeatures must be contained.
``some of``          at least one subfeatures must be contained.
``[<n> .. <m>] of``  at least ``n`` and at most ``m`` subfeatures must be contained.
===================  ======================================================================

.. note:: The ``all of``, ``one of`` and ``some of`` decompositions are also
   referred to as ``AND``, ``XOR`` and ``OR`` by some feature model formalisms.

A subfeature can be *optional*, which means it may or may not be included in the
feature combination. The ``optional`` keyword has a higher priority than the
feature decomposition. Consider the following example::

   root feature
       all of Producer, Consumer, optional Buffer;
   endfeature

Here, the ``all of`` decomposition states that all three subfeatures must be
included, but since ``Buffer`` is marked optional, there are two allowed
feature combinations: one that includes ``Buffer``, and one that does not.


.. _sec-multi-features:

Multi-features
--------------

ProFeat supports *multi-features*, i.e., features that can appear multiple
times in a feature combination. Multi-features are defined by specifying the
number of instances, as shown in the following example::

   feature Consumers
       some of Consumer[3];
   endfeature

   feature Consumer
       // ...
   endfeature

In this example, the ``Consumers`` feature contains 3 ``Consumer`` subfeatures.

.. note:: The number of instances can be any constant
   :ref:`expression <sec-expressions>`.

The individual feature instances are referenced by indexing. Thus, the
subfeatures of ``Consumers`` are ``Consumer[0]``, ``Consumer[1]`` and
``Consumer[2]`` (multi-features are 0-indexed).

If the ``optional`` keyword is applied to a multi-feature, then all feature
instances are optional, independently from each other.

The decomposition operator using a group cardinality (``[<n> .. <m>] of``)
counts the number of multi-feature instances and not the multi-feature itself.
Consider the following example::

   root feature
       [2..3] of Consumer[3];
   endfeature

This feature model specifies 4 feature combinations. One where all ``Consumer``
instances are included, and three where two of the ``Consumer`` instances are
selected.


Aliasing
--------

Sometimes a feature should be appear multiple times as a subfeature in the
same parent feature, but without making it a multi-feature. However, each
subfeature listed in a decomposition must be unique. Therefore, a feature may
be renamed upon referencing::

   feature Consumers
       one of Consumer as FirstConsumer, Consumer as SecondConsumer;
   endfeature

Here, the ``Consumer`` feature appears twice as a subfeature. To make the
feature instances unique, they are renamed using the ``as`` keyword. Thus,
``Consumers`` has the subfeatures ``FirstConsumer`` and ``SecondConsumer`` that
are both instances of the ``Consumer`` feature.

.. note:: The ``as`` keyword can also be used for multi-features. Then, the
   number of subfeatures is specified after the alias name, for instance
   ``Consumer as FastConsumer[3]``.


Qualified names
---------------

A reference to a feature instance may not always be unambiguous. Consider the
following example::

   root feature
       all of A, B, C[2];
   endfeature

   feature A
       all of X;
   endfeature

   feature B
       all of X;
   endfeature

   feature C
       all of Y;
   endfeature

   feature X endfeature
   feature Y endfeature

In the above example, there are two instances of the ``X`` feature. Thus, a
reference to ``X`` is ambiguous. The ambiguity can be resolved by qualifying the
feature instance name with its parent feature instance. Using the familiar dot
notation, the two ``X`` instances are referenced by ``A.X`` and ``B.X``.
Similarly, there are two instances of the ``Y`` feature, namely ``C[0].Y`` and
``C[1].Y``.

A *fully qualified name* is anchored on the root feature. For example, the
fully qualified name of the second ``X`` instance is ``root.B.X``.


Constraints
-----------

In addition to the constraints specified by the feature decomposition,
cross-tree constraints can be specified. A constraint is a Boolean expression
over the features in the feature model. If it evaluates to ``true`` for a given
feature combination, then this feature combination is allowed (or valid).
Consider the following example::

   root feature
       all of Producer, Consumers, Buffer, Fast;

       constraint active(Fast) => active(Consumer[0]) & active(Consumer[1]);
   endfeature

   feature Consumers
       some of Consumer[2];
   endfeature

This feature model specifies that both ``Consumer[0]`` and ``Consumer[1]`` must
be contained in the feature combination if ``Fast`` is contained. The
``active`` function returns ``true`` if a given feature is part of the feature
combination.

.. note:: Constraints can appear in any feature block. There are no restrictions
   regarding the location of constraints in the feature model. However, it is
   good practice to specify constraints as local as possible.

Constraints must hold even after
:ref:`dynamic feature switches <sec-controller>`. To specify constraints that
should only hold in the initial state, the ``initial`` keyword is used
(``initial constaint ...``).


Attributes
----------

ProFeat has support for *feature attributes* (sometimes called numerical
features). Feature attributes are part of the feature combination. An attribute
is defined within a ``feature`` block::

   feature Consumer
       speed : [0 .. 5];
   endfeature

In the above example, the ``Consumer`` feature has the attribute ``speed``.
An attribute can have any variable type: bounded integer, ``bool`` or an
``array`` type.

Attributes can also be constrained using feature constraints, as shown in the
following example::

   feature Consumers
       all of Consumer[2];

       constraint Consumer[0].speed + Consumer[1].speed < 7;
   endfeature

   feature Consumer
       speed : [0 .. 5];
   endfeature

The constraint states that the combined ``speed`` of the ``Consumer`` features
must be less than 7.


.. _sec-parametrization-feature:

Parametrization
---------------

Feature definitions can be parametrized to facilitate reuse and avoid code
duplication. The parameters of a feature are listed after the feature name::

   feature Consumer(max_speed)
       speed : [0 .. max_speed];
   endfeature

The parameters can be used anywhere in the feature block, including
:ref:`module <sec-modules>` references, :ref:`rewards <sec-rewards>`,
constraints and attributes (as shown above).

Arguments for a parametrized feature are provided when instantiating the
feature::

   root feature 
       all of Consumer(3) as Slow, Consumer(5) as Fast;
   endfeature

Here, the ``Consumer`` feature is instantiated twice with different argument
values. Note that the ``as`` keyword must be used here to given the instances
different names.
