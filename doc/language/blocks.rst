Block syntax
============

The ProFeat language uses the same syntax for blocks as the PRISM language. For
example, a feature module is defined by a ``module`` block::

   module my_module
       // module body
   endmodule

Alternatively, ProFeat allows curly braces (like the C language, for instance)
to denote blocks::

   module my_module {
       // module body
   }

PRISM-style blocks and C-style blocks can be used interchangeably and even be
mixed in the same model file.
