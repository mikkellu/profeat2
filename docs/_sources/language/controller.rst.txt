.. _sec-controller:

Dynamic feature switches
========================

A ProFeat model describes a family of systems where each member of the family
corresponds to a feature combination. These feature combinations are static,
i.e., once a feature combination has been selected it remains fixed. However,
ProFeat also supports dynamic switches between feature combinations. This is
especially useful for modeling adaptive systems or dynamic software product
lines.


Feature controller
------------------

The rules for switching between different configurations are specified by a
*feature controller*, which is an automata-based component and defined similarly
to :ref:`feature modules <sec-module-definitions>`. A controller is defined
using a ``controller`` block and may contain variables and commands. Consider
the following example::

   root feature
       all of Producer, Consumers, Buffer;
   endfeature

   feature Consumers
       some of Consumer[3];
   endfeature

   feature Consumer
       // ...
   endfeature

   controller
       [] buffer_full & !active(Consumer[2]) -> activate(Consumer[2]);
       [] buffer_low & active(Consumer[2]) -> deactivate(Consumer[2]);
   endcontroller

Here, the controller activates an additional ``Consumer`` in case
the buffer is full. Once the buffer is almost empty again, the additional
``Consumer`` is deactivated.

Inside a ``controller`` block, the special ``activate`` and ``deactivate``
updates can be used to activate or deactivate a feature, respectively. There
may be multiple ``activate`` and ``deactivate`` updates in a single command.
However, ``activate`` and ``deactivate`` cannot be used in stochastic updates.
Furthermore, command containing updates to the feature combination can only be
executed if it does not violate any of the constraints imposed by the
:ref:`feature model <sec-feature-model>`.

.. note:: The feature controller is optional, but there can be at most one
   feature controller in a ProFeat model.


Synchronization
---------------

A feature module can synchronize with the feature controller on the ``active``
and ``deactivate`` updates using the special ``activate`` and ``deactivate``
actions. In the following example, the feature module implementing the
``Consumer`` feature uses the ``deactivate`` action to block the controller
from deactivating it if it is not idle::

   module consumer_impl
       work : [0..MAX_WORK] init 0;

       // ...
       [deactivate] work = 0 -> true;
   endmodule

The module will only synchronize with the feature controller if no further work
must be processed.

Synchronization with the ``activate`` action can, for example, be used to
initialize the module's state upon activation.
