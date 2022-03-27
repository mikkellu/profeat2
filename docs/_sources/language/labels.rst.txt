.. _sec-labels:

Labels
======

Like PRISM models, ProFeat models can also contain labels to identify certain
sets of states. Labels are defined using the ``label`` keyword and must have type
``bool``, as illustrated in the following example::

   label "idle" = for i in [0..2] Consumer[i].work = 0 & ... endfor;
