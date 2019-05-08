.. _sec-rewards:

Costs and rewards
=================

In order to reason about quantitative measures, such as expected energy
consumption, expected time or expected number of lost messages, a ProFeat model
can be augmented with costs and rewards. Rewards are defined in reward
structures and can be assigned to states, transitions or feature switches.
Reward structures are defined inside of ``feature`` blocks to allow for
modularized cost models.

Consider the following example::

   feature Consumer
       rewards "energy"
           work = 0 : 1;
       endrewards
   endfeature

Here, a reward of 1 is assigned to all states where ``work`` is 0. The reward
structure has the label ``"energy"``. All rewards with the same label are
accumulated. The reward does not need to be constant, but can also depend on the
model state and feature attributes::

   feature Consumer
       speed : [1 .. 5];

       rewards "energy"
           work > 0 : pow(speed, 2);
       endrewards
   endfeature

Rewards can be assigned to transitions by adding a (possibly empty) action
label, as shown in the following example::

   rewards "energy"
       [work] active(this) : 1;
   endrewards

The reward item assigns a reward of 1 to all transitions labeled with the
action ``work``. The ``active`` function can also be used in reward definition.
The ``this`` keyword refers to the feature instance.

Rewards can also be assigned to feature switches by using the special
``activate`` and ``deactivate`` actions (see also
:ref:`feature controller <sec-controller>`). Consider the following example::

   feature Consumer
       rewards "energy"
           [activate] true : 2;
       endrewards
   endfeature

Here, a cost of 2 is assigned for every activation of the ``Consumer`` feature.

The :ref:`meta-programming <sec-meta-programming>` constructs can also be used
in reward structures to generate reward items::

   rewards "energy"
       for i in [0..2]
           [dequeue[i]] true : 1;
       endfor
   endrewards
