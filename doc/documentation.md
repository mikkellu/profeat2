ProFeat Documentation
=====================

Input Language
--------------

This section gives an overview of the ProFeat input language.

A ProFeat model is usually defined in two parts: The declaration of all
[Features][] that make up the product line, and a behavioural specification in
the form of [Feature Modules][] and an optional [Feature Controller][].

The ProFeat syntax mostly follows the syntax of the PRISM input language.
In the following, all additional constructs of ProFeat are described.

### Family declaration

Implicitly, a family defined by a set of feature declarations (see
[Features][]) consists of of all valid feature combinations and attribute
valuations (i.e. products). However, a model family can also be declared via an
explicit family declaration:

    family
        n : [0..3];
        b : bool;
        initial constraint b => n != 1;
    endfamily

This family-block introduces two parameters `n` and `b` as well as an initial
constraint to define the set of possible parameter valuations.

Parameters are constant for any given model instance and can thus be used
everywhere a constant may appear. Most importantly, this includes array sizes,
number of multi-features and bounds of for-loops. This means a family
declaration can be applied to parametrize the structure of a model.

Note: if family-based translation is enabled (which is the default), a parameter
used as a constant always has its upper bound as its value on translation.

### Features

In general, a feature is declared as follows:

    feature <identifier>[(<parameter1>, <parameter2>, ...)]
        [<attributes>]
        [<decomposition>]
        [<constraints>]
        [<blocking-actions>]
        [<feature-modules>]
        [<rewards>]
    endfeature

#### Attributes

A feature may have one or more attributes. They are similar to [Variables][],
but can only be modified by the [Feature Controller][].

#### Decomposition

A feature can be *decomposed* into one or more subfeatures.
ProFeat provides the decomposition operators `all` (all subfeatures are
required), `one` (exactly one subfeature may be active) and `some` (at least
one subfeature is required).

Consider the following example:

    feature my_feat
        some of sub_feat_x, sub_feat_y;
    endfeature

If the feature `my_feat` is part of the feature combination, then at least one
of `sub_feat_x` and `sub_feat_y` must be part of the feature combination as
well.

For more control over the number of subfeatures, a group cardinality can be
specified:

    feature my_feat
        [2..3] of a, b, c, d;
    endfeature

In this example, at least two, but at most three of the features `a`, `b`, `c`,
`d` must be part of the feature combination.

#### Multi-Features and Feature Instances

ProFeat allows to specify *multi-features* (features that can be part of a
feature combination more than once). In the following example, the feature
`my_multi_feature` is instantiated twice:

    feature my_feat
        [2..3] of my_multi_feature[2], sub_feat;
    endfeature

Instances of multi-features are 0-indexed.

In a decomposition with group cardinalities, the number of *instances* is
counted, not the number of features. In the above example, the following
feature combinations would be valid:

    { my_multi_feature[0], my_multi_feature[1] },
    { my_multi_feature[0], sub_feat },
    { my_multi_feature[1], sub_feat },
    { my_multi_feature[0], my_multi_feature[1], sub_feat }

#### Constraints

The set of all valid feature combinations defined by the feature model can be
further constrained by arbitrary constraints. A constraint is defined over the
feature instances of the feature model, e.g.:

    feature my_feat
        some of a[2], b, c;

        constraint active(b) => (active(a[0]) & active(c));
    endfeature

If feature `b` is part of the feature combination, then both `a[0]` and `c` must
also be part of the feature combination.

A `constraint` must always hold and is therefore enforced in both static and
dynamic product lines. However, if it is only necessary to constrain the
initial configurations of the product line, an `initial constraint` can be
specified:

    feature my_feat
        some of a[2], b, c;

        initial constraint active(b) => (active(a[0]) & active(c));
    endfeature

Here, the constraint only holds for the initial configuration, but the
[Feature Controller][] is free to establish a feature combination that violates
this constraint.

#### Referencing Feature Modules

A feature can be linked to one or more [Feature Modules][], that "implement"
the feature:

    feature my_feat
        modules my_feat_impl1, my_feat_impl2;
    endfeature

#### Rewards

ProFeat allows a modular specification of the reward/cost model. One or more
`rewards` structures can be placed inside a feature declaration:

    feature my_feat
        rewards "time"
            [tick] true : 1;
        endrewards

        rewards "resets"
            [] reset : 1;
        endrewards
    endfeature

#### Parametrization

Features are parametrizable. The parameters can be used anywhere (or more
precisely, in any expression) in the body of a feature declaration, e.g.:

    feature my_feat(instances, cost)
        [2..instances+2] a, b, c[instances];

        modules my_feat_impl(instances);

        rewards
            [] true : cost;
        endrewards
    endfeature

### Feature Modules

A feature module has the general form:

    module <identifier>[(<parameter1>, <parameter2>, ...)]
        <variable-declarations>

        <commands>
    endmodule

#### Variables

Variables are defined like in the PRISM language. In addition to the primitive
types ProFeat supports arrays:

    module my_module
        x : bool init true;
        y : [-1 .. 1] init 0;
        z : array [1..3] of bool init true;
    endmodule

Here, `z` is a 1-indexed array with 3 elements of type `bool` and each element
is initialized with `true`.

If no initial value is given for a variable, one of the possible values is
chosen nondeterministically (i.e. the model has multiple initial states). An
`init` block can be defined to constrain the set of initial states:

    module my_module
        x : bool;
        y : [-1 .. 1];
    endmodule

    init
        x => y = 0
    endinit

Since both `x` and `y` are left uninitialized their value is chosen
nondeterministically. However, if `x = true` then `y = 0` because all initial
states must satisfy the expression given in the `init` block.

#### Commands

Actions can be indexed, e.g.:

    module my_module(x)
        [act[0]]   true -> true;
        [act[x+1]] true -> true;
    endmodule

The built-in function `active(f)` returns `true`, if the given feature instance
is activated.

The built-in actions `activate` and `deactivate` can be used to synchronize the
feature module with the [Feature Controller][] in case the implemented feature
is activated or deactivated.

    feature my_feat
        modules my_module;
    endfeature

    module my_module
        busy : bool;

        [deactivate] !busy -> true;
    endmodule

In this example, the feature `my_feat` can only be deactivated by the
controller if the module is not `busy`.

By default, actions are not blocked if the feature is deactivated. In case this
is undesired, the `block` keyword followed by a list of actions can be used to
mark actions as blocking.

    feature my_feature
        block tock;
        modules my_module;
    endfeatur

    module my_module
        s : bool;

        [tick] !s -> (s' = true);
        [tock]  s -> (s' = false);
    endmodule

Here, the `tock` action is blocked if `my_feature` is not active.

### Feature Controller

The feature controller is optional and has the following general form:

    controller
        <variable-declarations>

        <commands>
    endcontroller

Variables and commands are defined like in a feature module. In addition to
variable updates, the controller can alter the feature combination using the
`activate` and `deactivate` updates:

    feature root
        some of a, b;
    endfeature

    feature a endfeature
    feature b endfeature

    controller
        [switch] true -> deactivate(a) & activate(b);
    endcontroller

A command which attempts to change the feature combination is automatically
blocked if the new configuration would violate any of the constraints defined
by the feature model.

### Metaprogramming

ProFeat provides a loop-construct to generate code.

A for-loop has the following general form:

    for <loop-variable> in [<from> .. <to>]

    endfor

A for-loop can be used in any expression, e.g.:

    formula fac(n) = for i in [1..n] { i * ... };

A loop can surround a block of commands...

    for i in [0..5]
        [] x = i -> (x' = i + 1);
    endfor

...distributions...

    [] x = 0 -> for i in [0..3] { 0.25:(x' = i) };

...and variable updates

    [] x = 0 -> 0.5:for i in [0..3] { (y[i]' = 0) } + 0.5:true;

### Invariants

Similar to an `init` block, which constrains the initial states of a model, an
`invariant` block constrains the set of reachable states (this includes the
initial states).

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

In this example, the first command of `my_module` will never be executed, since
adding 1 to `x` would violate the invariant. However, the second command is
not affected, as it does not lead to a violation of the invariant.

Running ProFeat
---------------

Given a ProFeat model `my_model.profeat` and a properties list
`my_props.fprops`, ProFeat can be run from the command line as follows:

    profeat my_model.profeat my_props.fprops

ProFeat will translate both the model and the properties into the PRISM
input language, run PRISM with these translated files and postprocess the
output of PRISM.

If you just want to translate the model (and properties) without running
PRISM, supply the -t switch and file names for the generated output:

    profeat my_model.profeat -t -o my_model.prism
    profeat my_model.profeat my_props.fprops -t -o my_model.prism -p my_props.props


Assuming you have written the PRISM output to `out.log`, you can postprocess
the results by importing `out.log`:

    profeat my_model.profeat my_props.fprops --import-results out.log

For further options, see

    profeat --help

