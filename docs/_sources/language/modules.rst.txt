.. _sec-modules:

Feature modules
===============

A *feature module* defines the behavior (or part of the behavior) of a feature.


Linking features and modules
----------------------------

A feature is implemented by referencing one or more modules after the
``modules`` keyword inside the ``feature`` block::

   feature Consumer
       modules consumer_impl;
   endfeature

   module consumer_impl
       // ...
   endmodule

In this example, the ``Consumer`` is implemented by the ``consumer_impl``
module. Multiple modules can be listed after the ``modules`` keyword (separated
by comma).

.. note:: A module can be referenced by multiple features. In this case, the
   module is instantiated multiple times.


.. _sec-module-definitions:

Module definitions
------------------

A module consists of two parts. The *variables* define its possible states and
*commands* specify the behavior by transitions between these states.

For example, the consumer may have a variable that stores the amount of
remaining work to be processed::

   module consumer_impl
       work : [0 .. 3] init 0;
   endmodule

.. note:: If no initial value is defined, the initial value for the variable is
   chosen nondeterministically and the model has
   :ref:`multiple initial states <sec-init>`.

A variable can have one of the following types.

``[<n> .. <m>]``
   A bounded integer with minimum value ``n`` and maximum value ``m`` (the
   upper bound is inclusive).

``bool``
   A Boolean variable which can be either ``true`` or ``false``.

``array [<n> .. <m>] of <type>``
   An array with indices ranging from ``n`` to ``m`` (inclusive). The element
   type must be either ``bool`` or a bounded integer. An array can be
   initialized using an array literal::

      x : array [0..2] of bool init {true, false, true};

   For initializing all elements with the same value, the following short-hand
   notation can be used::

      x : array [0..2] of bool init false;

A command comprises a *guard* and an *update*. If the guard evaluates to
``true`` in the current state the command may be executed, which updates the
local variables of the module. In the following example, the command processes
the remaining work by decreasing the ``work`` variable::

   module consumer_impl
       work : [0 .. 3] init 0;

       [] work > 0 -> (work' = work - 1);
   endmodule

Module variables are defined in the scope of its feature. Thus, other modules
can refer to the ``work`` variable of the ``Consumer`` feature by
``Consumer.work``.

.. note:: In contrast to the PRISM language, module variables in ProFeat do not
   have to be globally unique. They only must be unique within the module
   definition. However, the variables of two modules that implement the same
   feature must be distinct, since the scope is defined by the feature, not the
   module.

Each time a feature is instantiated, its associated feature modules are
instantiated with it. In case the feature module implements a multi-feature,
the ``id`` keyword refers to the index of the corresponding feature instance.
Consider the following example::

   feature A
       all of X[2];
   endfeature

   feature X
       module X_impl;
   endfeature

   module X_impl
       y : [0 .. 1] init id;
   endmodule

The variable ``y`` is initialized with the index of the ``X`` feature instance,
i.e., ``X[0].y`` is initialized with 0 and ``X[1].y`` is initialized with 1.


Synchronization
---------------

Commands can be labeled with *actions*. Actions can be used to force two or
more modules to take their transitions simultaneously, i.e., to *synchronize*.
Actions are placed in the square brackets before a command::

   module consumer_impl
       work : [0 .. 3] init 0;

       [dequeue] work = 0 -> (work' = 3);
   endmodule

In the above example, the ``Consumer`` feature synchronizes with the buffer over
the ``dequeue`` action to fetch the next work package.

Actions can be indexed similar to arrays using the index operator ``[ ]``. For
instance, the above module definition can be changed such that each instance
of a ``Consumer`` multi-feature has its own ``dequeue`` action by using the
``id`` as the index::

   module consumer_impl
       work : [0 .. 3] init 0;

       [dequeue[id]] work = 0 -> (work' = 3);
   endmodule

If a feature is not part of a feature combination, then its modules are not
included in the model instance for this feature combination. This means that an
inactive feature (or rather, its modules) will not block any of its actions.
However, sometimes it is desired that deactivating a feature blocks some or all
of its actions. For example, the ``dequeue`` action of the ``Consumer`` feature
should block if the ``Consumer`` feature is not in the feature combination.
Otherwise, the buffer could execute this action on its own, which effectively
drops the work package. Actions that should block are listed after the ``block``
keyword in the ``feature`` block::

   feature Consumer
       block dequeue[id];

       modules consumer_impl;
   endfeature


Parametrization
---------------

Similar to :ref:`features <sec-parametrization-feature>`, modules can be
parametrized. For example, a buffer implementation can be parametrized by the
buffer size::

   module buffer(capacity)
       cell : array [0..capacity - 1] of bool init false;

       // ...
   endmodule

Arguments are provided upon instantiation of the module in a ``feature`` block::

   feature SmallBuffer
       modules buffer(2);
   endfeature

A features' parameters can be used as arguments as well::

   feature Buffer(capacity)
       modules buffer(capacity);
   endfeature


.. _sec-meta-programming:

Meta-programming
----------------

ProFeat provides the ``for`` loop construct to generate sequences of commands,
stochastic updates, variable assignments and expressions. Consider the following
example of the ``buffer`` module, which implements a FIFO buffer with
parametrized capacity::

   module buffer(capacity)
       cell : array [0..capacity - 1] of [-1..MAX_WORK] init -1;

       for c in [0..2]
           [dequeue[c]] cell[0] != -1 ->
               (cell[capacity-1]' = -1) &
               for i in [0..capacity-2]
                   (cell[i]' = cell[i+1])
               endfor;
       endfor
   endmodule

The outer ``for`` loop generates a command for each instance of the ``Consumer``
:ref:`multi-feature <sec-multi-features>`. The inner loop then shifts the work
items in the buffer upon dequeuing an item.

If the ``for`` construct is used in an expression, the loop body must contain
exactly one ``...`` placeholder. Intuitively, the placeholder is the position
where the expression generated by the remaining iterations is inserted. Consider
the following expression containing a ``for`` loop::

   for i in [1..4] i + ... endfor

In the first iteration, this expression is expanded to::

   1 + for i in [2..4] i + ... endfor

Unfolding the loop completely yields the expression::

   1 + (2 + (3 + 4))

.. note:: The placeholder ``...`` can only be used with the operators
   ``+``, ``-``, ``*``, ``&`` and ``|`` as well as the functions ``min`` and
   ``max``.
