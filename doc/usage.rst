Running ProFeat
===============

This section describes ProFeat's command line interface and the various options
to configure the translation, analysis and post-processing steps.


Basic usage
-----------

ProFeat can be run from the command line and must be invoked with a file
containing a ProFeat model and a properties file::

   profeat my_model.profeat my_props.fprops

This will translate the model, invoke PRISM and show a table with the analysis
results. If ProFeat is invoked without a properties file, the model will only
be translated and then built using PRISM, but no analysis is performed.

To perform only the translation, but not the analysis using PRISM, the ``-t``
option can be used::

   profeat my_model.profeat my_model.fprops -t

This command will produce the output files ``out.prism``  and ``out.props``
containing the translated model and properties, respectively. To specify a
different name for the PRISM model file, the ``-o`` option is used::

   profeat -t my_model.profeat -o my_model.prism

The above command will write the translated model to ``my_model.prism``.
Similarly, the ``-p`` option is used to specify a path for the translated
properties file.


.. _sec-options:

Options
-------

``--one-by-one``
   By default, ProFeat encodes the whole system family into a single PRISM
   model. Alternatively, all family members can be analyzed one-by-one, i.e.,
   PRISM is invoked for each instance of the system family separately.

   If the ``--one-by-one`` option is used in combination with any other
   option that produces output files, e.g., the ``-o`` option, an index is
   automatically appended to the file names. For example, ProFeat may be invoked
   using the following options::

      profeat my_model.profeat -o my_model.prism --one-by-one

   This will produce the files ``my_model_0.prism``, ``my_model_1.prism``,
   ``my_model_2.prism`` and so on for each family instance.

``--constant-folding``
   Evaluates all constant subexpressions in the translated model as far as
   possible.

``-o <FILE>`` or ``--export-model <FILE>``
   Exports the translated model to the specified file.

``-p <FILE>`` or ``--export-properties <FILE>``
   Exports the translated properties to the specified file.

``--export-fd <FILE>``
   Exports a feature diagram for the feature model defined in the ProFeat model.
   The exported diagram is exported in the Graphviz DOT format.

``--export-vars <FILE>``
   Exports the mapping of fully qualified feature names in the ProFeat model
   to the variable names used in the PRISM model. The mapping is exported in
   the CSV format.

``-r <FILE>`` or ``--export-results <FILE>``
   Exports the analysis results in CSV format.

``--export-time <FILE>``
   Exports build time, model checking time and other time counters reported by
   PRISM in CSV format.

``--above-threshold <NUMBER>``
   Computes a propositional logic constraint over the features in the model.
   The constraint evaluates to ``true`` for all feature combinations whose
   analysis result is above the given threshold.

``--export-mtbdd <FILE>``
   Exports a multi-terminal binary decision diagram representing the analysis
   results in the Graphviz DOT format.

   The rounded nodes in the diagram represent features and the rectangular
   nodes represent the analysis results. The diagram is read from top to
   bottom. A solid line means the feature is selected, a dashed line means it is
   not selected. A path from the root node to a leaf node provides the analysis
   result for a single feature combination.

``--full-mtbdd``
   By default, the MTBDD exported by the ``--export-mtbdd`` option does not
   include the constraints for the initial configurations. Using this option,
   the full MTBDD can be exported. It contains an additional (empty) leaf node,
   which stands for an invalid feature combination.

``--reorder-mtbdd``
   Tries to reduce the size of the MTBDD exported by the ``--export-mtbdd``
   option via reordering.

``--round-results <NUMBER>``
   Rounds the analysis results to the specified precision (number of fractional
   digits).

``--prism-log``
   Prints the log messages of PRISM to stdout.

``--export-log <FILE>``
   Exports the PRISM log to the specified file.

``--import-results <FILE>``
   Imports a PRISM log (from an all-in-one analysis) or multiple PRISM logs
   (from a one-by-one analysis) for post-processing.

   Using this option, the three steps performed by ProFeat (translation,
   invoking PRISM, post-processing) can be performed stepwise::

      profeat -t my_model.profeat my_model.fprops
      prism out.prism out.props > out.log
      profeat my_model.profeat my_model.fprops --import-results out.log

   Note that the same model file and properties file must be used on the second
   invocation of ProFeat. The three commands above are equivalent to::

      profeat my_model.profeat my_model.fprops

   The ``--import-results`` option can also be used with a one-by-one analysis::

      profeat -t my_model.profeat my_model.fprops --one-by-one
      for i in $(seq 0 N); do prism out_${i}.prism out.props > out_${i}.log; done
      profeat my_model.profeat my_model.fprops --import-results out.log

   Note that ProFeat automatically inserts the indices in the ``out.log`` file
   name upon importing the PRISM log files.

``-t`` or ``--translate``
   Only translates the model, but performs no analysis.

``-m`` or ``--model-checking``
   Performs the translation and the analysis, but not the post-processing step.

``--prism-path``
   Set the path to the PRISM startup script that should be used to invoke
   PRISM. Default: ``prism``.

``--prism-args <ARGS>``
   Additional arguments that should be passed to PRISM. Note that the arguments
   should be quoted, so that they do not interfere with ProFeat's arguments,
   for example::

      profeat my_model.profeat my_model.fprops --prism-args "-m -cuddmaxmem 2g"
