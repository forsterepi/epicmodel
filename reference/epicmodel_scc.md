# SCC model objects

The S3 class `epicmodel_scc` is used to store information on
sufficient-component cause (SCC) models created by
[`create_scc()`](https://forsterepi.github.io/epicmodel/reference/create_scc.md).

`new_scc()`, `validate_scc()`, and `empty_scc()` define the S3 class.

[`print()`](https://rdrr.io/r/base/print.html) prints a summary of SCC
models in the console.
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`print()`](https://rdrr.io/r/base/print.html) are identical.

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) creates the
familiar causal pie charts from an object of class `epicmodel_scc`.

## Usage

``` r
new_scc(x = list())

validate_scc(x)

empty_scc()

# S3 method for class 'epicmodel_scc'
print(x, ...)

# S3 method for class 'epicmodel_scc'
summary(object, ...)

# S3 method for class 'epicmodel_scc'
plot(
  x,
  remove_sc = NULL,
  sc_label = NULL,
  unknown = TRUE,
  names = TRUE,
  text_color = NULL,
  pie_color = NULL,
  border_color = NULL,
  ...
)
```

## Arguments

- x:

  `x` is used in several functions:

  - `new_scc()`: A list to be converted to class `epicmodel_scc`.

  - `validate_scc()`: An object of class `epicmodel_scc` to be
    validated.

  - `print.epicmodel_scc()`: An object of class `epicmodel_scc`.

  - `plot.epicmodel_scc()`: An object of class `epicmodel_scc`.

- ...:

  Additional arguments for generics
  [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html), and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

- object:

  For `summary.epicmodel_scc()`, an object of class `epicmodel_scc`.

- remove_sc:

  For `plot.epicmodel_scc()`, a vector of integerish numbers, i.e.,
  integers that can be specified as numeric, i.e., `1` and `1L` are both
  possible. Removes the sufficient cause (SC) with the specified index
  from the plot, i.e., for `remove_sc = 2`, removes SC 2, and for
  `remove_sc = c(2,3)`, removes SC 2 and SC 3. If there are x sufficient
  causes in the model, x is the highest allowed value. At least one
  sufficient cause needs to remain, i.e., not all sufficient causes can
  be removed. If NULL (default), all sufficient causes are plotted.

- sc_label:

  For `plot.epicmodel_scc()`, a character vector with the labels written
  above the pies, i.e., sufficient causes. If NULL (default),
  "Sufficient Cause 1", "Sufficient Cause 2", etc. are used. If
  specified, try to provide as many labels as there are pies in the
  plot. Duplicates are not allowed.

- unknown:

  For `plot.epicmodel_scc()`, TRUE (default) or FALSE. If TRUE, unknown
  causes are added to the SCC model: every sufficient cause gets an
  additional individual unknown component cause representing additional
  unknown components; an unknown sufficient cause is added to the model
  consisting of a single unknown component cause and representing all
  unknown sufficient causes.

- names:

  For `plot.epicmodel_scc()`, TRUE (default) or FALSE. If TRUE, includes
  the translation of pie segment names to descriptions of component
  causes in the plot.

- text_color:

  For `plot.epicmodel_scc()`, a single element of type character, which
  is a valid color description. Valid color descriptions can be named
  colors ("white") or hexadecimal color codes ("#FFFFFF"). `text_color`
  will be used for the pie segment names. If NULL (default), "white" is
  used.

- pie_color:

  For `plot.epicmodel_scc()`, a character vector of length 3 containing
  valid color descriptions. Valid color descriptions can be named colors
  ("white") or hexadecimal color codes ("#FFFFFF"). The first element of
  `pie_color` is used to color sufficient causes, which are always
  sufficient. The second element is used to color sufficient causes, for
  which sufficiency depends on the order of occurrence. The third
  element is used to color the unknown sufficient cause, which is
  present if `unknown` is TRUE. If NULL (default), the following colors
  are used: "#B1934A", "#A65141", "#394165"

- border_color:

  For `plot.epicmodel_scc()`, a single element of type character, which
  is a valid color description. Valid color descriptions can be named
  colors ("white") or hexadecimal color codes ("#FFFFFF").
  `border_color` will be used for all pie borders apart from the unknown
  sufficient cause. Therefore, only specify `border_color` if `unknown`
  is FALSE. If NULL (default), "white" is used. (Borders for the unknown
  sufficient cause have the same color as the pie.)

## Value

- `new_scc()`: An object of class `epicmodel_scc`.

- `validate_scc()`: An object of class `epicmodel_scc` that has been
  checked to have the correct structure.

- `empty_scc()`: A (realtively) empty object of class `epicmodel_scc`
  with correct structure.

- `print.epicmodel_scc()`: Prints a summary of the object of class
  `epicmodel_scc` in the console.

- `summary.epicmodel_scc()`: Same as `print.epicmodel_scc()`.

- `plot.epicmodel_scc()`: A `ggplot` object.

## Details

### `epicmodel_scc` objects

`epicmodel_scc` objects are lists containing 10 elements. These elements
are described below:

- `sc_cc`:

  A data.frame with one column for every component cause and one row for
  every sufficient cause. Colnames are the step IDs from the
  corresponding steplist. Rownames are sufficient cause IDs (see below).
  Each cell contains either TRUE or FALSE indicating if the component
  cause in the column is part of a set of component causes described by
  the row.

- `sc_status`:

  A named character vector with one element for every sufficient cause.
  The names are sufficient cause IDs (see below). The elements contain
  the status of the sufficient cause (see below). Here, only "always",
  "depends", and "depends (potential order implausibilities)" appear.

- `sc_steps`:

  A list of character vectors with one list element for every sufficient
  cause. The list is named using sufficient cause IDs (see below). Every
  character vector contains the step IDs of all steps that are part of
  the corresponding sufficient cause, i.e., that can be caused by the
  corresponding set of component causes.

- `sc_order`:

  A list with one list element for every sufficient cause. The list is
  named using sufficient cause IDs (see below). List elements are either
  NA (if a sufficient cause's status is "always") or a data.frame (if a
  sufficient cause's status is "depends" or "depends (potential order
  implausibilities)". Data.frames contain two columns, which are called
  "order" and "suff" (short for "sufficient"), and one row for every
  order of occurrence. The order of occurrence is summarized in "order"
  (as character), while "suff" is either TRUE or FALSE indicating if the
  corresponding order of occurrence is sufficient, i.e., leads to the
  outcome, or not.

- `sc_implausibilities`:

  A named vector of TRUE and FALSE with length equal to the number of
  sufficient causes. The names are sufficient cause IDs (see below). Is
  TRUE if for the corresponding sufficient cause there are potential
  order implausibilities, i.e., if its status is "depends (potential
  order implausibilities)", and is FALSE otherwise.

- `sc_implausibilities_detail`:

  A list with one list element for every sufficient cause. The list is
  named using sufficient cause IDs (see below). List elements are either
  NA (if the corresponding element in `sc_implausibilities` is FALSE) or
  a character vector (if the corresponding element in
  `sc_implausibilities` is TRUE) with the THEN statements of the steps
  that might be involved in implausible orders of occurrence.

- `sc_use_modules`:

  Either TRUE or FALSE indicating if modules have been specified in the
  steplist.

- `unknown_cc`:

  Similar to `sc_cc` but includes unknown component causes and an
  unknown sufficient cause (see "Unknown causes" below). It therefore
  additionally contains:

  - one column to the right for every sufficient cause with name
    "U`rownumber`" (U1, U2, etc.) and all values equal to FALSE appart
    from row `rownumber`, which is TRUE

  - one additional column to the right with name "USC" and all values
    equal to FALSE for all sufficient causes

  - one additional row with name "cc0" and all values equal to FALSE
    apart from column "USC", which is TRUE

- `unknown_status`:

  Similar to `sc_status` but has one additional element with value
  "unknown" and name "cc0" (see "Unknown causes" below).

- `steplist`:

  The object of class `epicmodel_steplist_checked` that has been the
  input to function
  [`create_scc()`](https://forsterepi.github.io/epicmodel/reference/create_scc.md),
  from which the `epicmodel_scc` object has been created.

### Other details:

- `Sufficient cause IDs`:

  [`create_scc()`](https://forsterepi.github.io/epicmodel/reference/create_scc.md)
  checks every combination of component causes for sufficiency. Every
  combination is assigned an ID of the format "cc`number`" (cc1, cc2,
  etc.). `epicmodel_scc` only contains information about minimally
  sufficient combinations of component causes, but the initial IDs are
  kept. The IDs are used throughout the different elements of
  `epicmodel_scc` to link information that belongs to the same
  sufficient cause. The unknown sufficient cause used in elements
  `unknown_cc` and `unknown_status` has ID cc0.

- `Unknown causes`:

  Since many causes might be unknown, it is reasonable for some
  applications to include these unknown causes in a SCC model (see,
  e.g., Rothman et al. (2008)). They are also useful to remind us of our
  limited knowledge. In a suffcient-component cause model, unknown
  causes come in two flavors:

  - `Unknown component causes`: These are additional component causes
    within a sufficient cause, which are necessary for sufficiency.
    Please note that each sufficient cause has its own set of unknown
    component causes. In `unknown_cc`, unknown component causes are
    called U1, U2, etc.

  - `Unknown sufficient causes`: There might be unknown mechanisms that
    lead to outcome occurrence. These sufficient causes are summarized
    in one additional sufficient cause, which has only a single
    component cause called `USC` in `unknown_cc`. This set of component
    causes has sufficient cause ID `cc0`.

  Please note that in
  [`plot_dag()`](https://forsterepi.github.io/epicmodel/reference/plot_dag.md)
  an ellipse represents a **determinative set** of sufficient causes, as
  suggested and defined by VanderWeele & Robins (2007). A determinative
  set contains all sufficient causes and, therefore, in most cases, an
  unknown sufficient cause is necessary to at least achieve a
  theoretical determinative set. Determinative sets are important for
  creating causal diagrams (in the form of directed acyclic graphs) from
  SCC models. VanderWeele and Robins (2007) write (p. 1099, D refers to
  the outcome):

  *"To ensure that the DAG with the sufficient causation structure is
  itself a causal DAG, it is important that the set of sufficient causes
  for* *D on the graph be a determinative set of sufficient causes —
  that is, that the sufficient causes represent all of the pathways by
  which the* *outcome D may occur. Otherwise certain nodes may have
  common causes which are not on the graph, and the graph will then not
  be a causal DAG."*

  It can of course be argued that an unknown sufficient cause in the
  described form is hardly of any use when creating a causal graph (as a
  DAG) from a SCC model. Nonetheless, it can be, as mentioned, a
  placeholder and reminder of limited knowledge.

- `Sufficiency status`:

  The sufficiency status describes under which circumstances a certain
  set of component causes is sufficient. There are 5 possible values:

  - `always`: The set of component causes is always sufficient.

  - `depends`: The set of component causes is sometimes sufficient and
    sufficiency depends on the order of occurrence of the involved
    steps, because some of them contain IFNOT conditions. However, if an
    IFNOT condition prevents the step from happening depends on the
    order of occurrence: if the IF condition is fulfilled before the
    IFNOT condition, the step (usually) occurs anyways, similar to how I
    do not care if a door is closed if I already went through it when it
    was still open.

  - `depends (potential order implausibilities)`: Same as "depends", but
    in the list of potential orders of occurrence of the involved steps,
    there might be some that do not make sense in practice, e.g., when
    two steps with IFNOT conditions are chained together: Imagine Step1
    having IF condition If1 and IFNOT condition Ifnot1, and Step2 having
    IF condition If2 and IFNOT condition Step1. The order Step1 -\>
    Ifnot1 -\> If1 -\> If2 is not plausible because Ifnot1 occurred
    before If1 and therefore Step1 did never occur. The user needs to
    discard these orders of occurrence (as I am currently not confident
    to correctly remove only implausible ones with code).

  - `never`: The set of component causes is never sufficient. This
    status is not used in `epicmodel_scc`. It's only used when
    investigating the effect of interventions (see
    [`intervene()`](https://forsterepi.github.io/epicmodel/reference/intervene.md)).

  - `unknown`: This is the status of the unknown sufficient cause, which
    is added to the SCC model. It's only used in element
    `unknown_status` of `epicmodel_scc` objects.

## References

- Rothman KJ, Greenland S, Poole C, Lash TL (2008): Causation and Causal
  Inference. In: Rothman KJ, Greenland S, Lash TL (Ed.): Modern
  epidemiology. Third edition. Philadelphia, Baltimore, New York:
  Wolters Kluwer Health Lippincott Williams & Wilkins, pp. 5–31.

- VanderWeele TJ, Robins JM (2007): Directed acyclic graphs, sufficient
  causes, and the properties of conditioning on a common effect.
  American Journal of Epidemiology 166 (9): 1096–1104.

## See also

- [`create_scc()`](https://forsterepi.github.io/epicmodel/reference/create_scc.md)
  for information on the algorithm for creating SCC models

- [`plot_dag()`](https://forsterepi.github.io/epicmodel/reference/plot_dag.md)
  for how determinative sets of component causes are displayed in a DAG

- [`intervene()`](https://forsterepi.github.io/epicmodel/reference/intervene.md)
  for the use of sufficiency status "never"

## Examples

``` r
# epicmodel_scc object are created by create_scc()

# first, check your steplist of choice
steplist_checked <- check_steplist(steplist_rain)
#> 
#> ── Checking epicmodel_steplist steplist_rain ───────────────────────────────────
#> ✔ Checking WHAT IDs was successful.
#> ✔ Checking DOES IDs was successful.
#> ✔ Checking WHERE IDs was successful.
#> ✔ Checking Module IDs was successful.
#> ✔ Checking ICC IDs was successful.
#> ✔ Checking WHAT keywords was successful.
#> ✔ Checking DOES keywords was successful.
#> ✔ Checking WHERE keywords was successful.
#> ✔ Checking Module keywords was successful.
#> ✔ Checking Modules was successful.
#> ✔ Checking ICC entries was successful.
#> ✔ Checking WHAT segments was successful.
#> ✔ Checking DOES segments was successful.
#> ✔ Checking WHERE segments was successful.
#> ✔ Checking references was successful.
#> ✔ Checking start/end steps was successful.
#> ✔ Checking THEN statements was successful.
#> ✔ Checking THEN/IF/IFNOT equality was successful.
#> ✔ Checking outcome definitions was successful.
#> ── Summary ─────────────────────────────────────────────────────────────────────
#> ✔ Checking successful!
# then, use it in create_scc()
scc_model <- create_scc(steplist_checked)
#> 
#> ── Create SCC Model ──
#> 
#> ✔ 15/15 | Check if set of component causes is sufficient
#> ✔ 5/5 | Check if sufficiency dependends on IFNOT conditions
#> ✔ 5/5 | Check if sufficient cause is minimal
#> ℹ 2/5 sufficient causes are minimal

# new_scc() and validate_scc() are used inside create_scc()
# nonetheless, you can check its structure with validate_scc()
validate_scc(scc_model)
#> 
#> ── Outcome Definitions ──
#> 
#> • you get wet
#> 
#> ── SC 1 ──
#> 
#> ✔ Always sufficient
#> Component causes:
#> • rain
#> • get groceries
#> 
#> Modules
#> • activity: 50% (2/4)
#> • weather: 50% (2/4)
#> 
#> ── SC 2 ──
#> 
#> ✔ Always sufficient
#> Component causes:
#> • no vacation
#> • weekday
#> • rain
#> 
#> Modules
#> • activity: 50% (3/6)
#> • weather: 33% (2/6)
#> • fate: 17% (1/6)
#> 

# print() and summary() both summarize the model in the console
print(scc_model)
#> ── Outcome Definitions ──
#> 
#> • you get wet
#> 
#> ── SC 1 ──
#> 
#> ✔ Always sufficient
#> Component causes:
#> • rain
#> • get groceries
#> 
#> Modules
#> • activity: 50% (2/4)
#> • weather: 50% (2/4)
#> 
#> ── SC 2 ──
#> 
#> ✔ Always sufficient
#> Component causes:
#> • no vacation
#> • weekday
#> • rain
#> 
#> Modules
#> • activity: 50% (3/6)
#> • weather: 33% (2/6)
#> • fate: 17% (1/6)
#> 
scc_model
#> ── Outcome Definitions ──
#> 
#> • you get wet
#> 
#> ── SC 1 ──
#> 
#> ✔ Always sufficient
#> Component causes:
#> • rain
#> • get groceries
#> 
#> Modules
#> • activity: 50% (2/4)
#> • weather: 50% (2/4)
#> 
#> ── SC 2 ──
#> 
#> ✔ Always sufficient
#> Component causes:
#> • no vacation
#> • weekday
#> • rain
#> 
#> Modules
#> • activity: 50% (3/6)
#> • weather: 33% (2/6)
#> • fate: 17% (1/6)
#> 
summary(scc_model)
#> ── Outcome Definitions ──
#> 
#> • you get wet
#> 
#> ── SC 1 ──
#> 
#> ✔ Always sufficient
#> Component causes:
#> • rain
#> • get groceries
#> 
#> Modules
#> • activity: 50% (2/4)
#> • weather: 50% (2/4)
#> 
#> ── SC 2 ──
#> 
#> ✔ Always sufficient
#> Component causes:
#> • no vacation
#> • weekday
#> • rain
#> 
#> Modules
#> • activity: 50% (3/6)
#> • weather: 33% (2/6)
#> • fate: 17% (1/6)
#> 

# plot causal pies with plot()
plot(scc_model)

```
