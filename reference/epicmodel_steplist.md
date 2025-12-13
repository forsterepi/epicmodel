# Steplist objects

The S3 classes `epicmodel_steplist` and `epicmodel_steplist_checked`
store the input information for SCC model creation. They are created
from the Steplist Creator `shiny` app, which can be launched with
[`launch_steplist_creator()`](https://forsterepi.github.io/epicmodel/reference/launch_steplist_creator.md).

`new_steplist()`, `validate_steplist()`, and `empty_steplist()` define
the S3 class.

[`print()`](https://rdrr.io/r/base/print.html) prints a summary of the
steplist entries in the console.

[`summary()`](https://rdrr.io/r/base/summary.html) prints a list of
steps sorted by type of step in the console.

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) renders a graph
of the complete network of mechanisms in the RStudio Viewer.

## Usage

``` r
new_steplist(x = list())

validate_steplist(x)

empty_steplist()

# S3 method for class 'epicmodel_steplist'
print(x, ...)

# S3 method for class 'epicmodel_steplist_checked'
print(x, ...)

# S3 method for class 'epicmodel_steplist'
summary(object, ...)

# S3 method for class 'epicmodel_steplist_checked'
summary(object, ...)

# S3 method for class 'epicmodel_steplist'
plot(x, ...)

# S3 method for class 'epicmodel_steplist_checked'
plot(x, modules = TRUE, module_colors = NULL, render = TRUE, ...)
```

## Arguments

- x:

  `x` is used in several functions:

  - `new_steplist()`: A list to be converted to class
    `epicmodel_steplist`.

  - `validate_steplist()`: An object of class `epicmodel_steplist` or
    `epicmodel_steplist_checked` to be validated.

  - `print.epicmodel_steplist()`: An object of class
    `epicmodel_steplist`.

  - `print.epicmodel_steplist_checked()`: An object of class
    `epicmodel_steplist_checked`.

  - `plot.epicmodel_steplist()`: An object of class
    `epicmodel_steplist`.

  - `plot.epicmodel_steplist_checked()`: An object of class
    `epicmodel_steplist_checked`.

- ...:

  Additional arguments for generics
  [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html), and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

- object:

  For `summary.epicmodel_steplist()`, an object of class
  `epicmodel_steplist`. For `summary.epicmodel_steplist_checked()`, an
  object of class `epicmodel_steplist_checked`.

- modules:

  For `plot.epicmodel_steplist_checked`, TRUE (default) or FALSE,
  indicating if nodes in the same module should be colored equally
  (TRUE) or if all nodes have white background (FALSE). Colors are only
  applied, if modules have actually been specified in the
  `epicmodel_steplist`.

- module_colors:

  For `plot.epicmodel_steplist_checked`, if nodes are colored by module,
  colors can be provided via this argument. Colors must be provided as a
  character vector. Both named colors and hexadecimal color codes are
  allowed. The function has 8 colors stored internally. If
  `module_colors` = NULL (default), these colors are used. If the model
  has more than 8 modules, `module_colors` must be specified. If more
  colors than necessary are specified, the function takes as many as
  necessary from the start of the vector.

- render:

  For `plot.epicmodel_steplist_checked`, if TRUE (default), graph is
  directly rendered. IF FALSE, the output contains the non-rendered
  graph.

## Value

- `new_steplist()`: An object of class `epicmodel_steplist`.

- `validate_steplist()`: An object of class `epicmodel_steplist` or
  `epicmodel_steplist_checked`, that has been checked to have the
  correct structure.

- `empty_steplist()`: An empty object of class `epicmodel_steplist`
  object with correct structure.

- `print.epicmodel_steplist()`: Prints the number of entries in each
  data.frame in the console and the information that the steplist is
  `unchecked`.

- `print.epicmodel_steplist_checked()`: Same as
  `print.epicmodel_steplist()` but with the information the the steplist
  has been checked successfully.

- `summary.epicmodel_steplist()`: Prints an allert that the steplist
  needs to be checked with
  [`check_steplist()`](https://forsterepi.github.io/epicmodel/reference/check_steplist.md)
  before using [`summary()`](https://rdrr.io/r/base/summary.html).

- `summary.epicmodel_steplist_checked()`: Prints a list of steps by type
  of step in the console.

- `plot.epicmodel_steplist()`: Prints an allert that the steplist needs
  to be checked with
  [`check_steplist()`](https://forsterepi.github.io/epicmodel/reference/check_steplist.md)
  before using [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

- `plot.epicmodel_steplist_checked()`: Prints a graph of the complete
  network of mechanisms in the RStudio Viewer and the corresponding
  legend in the console.

## Details

### `epicmodel_steplist` objects

`epicmodel_steplist` objects are lists containing 8 data.frames. These
data.frames are described below:

- `what`:

  A list of subjects and objects (WHAT segments) appearing in the step
  descriptions, e.g., cells, interleukins, symptoms, etc., with the
  following variables:

  - id_what: Automatically created ID for WHAT segments. Starts with "a"
    followed by a number, e.g., a1. Used in creating automatic step IDs.

  - key_what: Keyword describing the WHAT segment. Used in
    steplist_creator shiny app dropdown menus.

  - desc_what: Text used in step descripiton.

  - plural_what: Indicates if plural (1) or singular (0) version of the
    DOES description should be used, if this WHAT segment is used as
    subject, i.e., WHAT segment before the DOES segment.

- `does`:

  A list of actions or verbs (DOES segments), with which the WHAT
  segments interact, e.g., is present, produce, migrate, exposed to,
  with the following variables:

  - id_does: Automatically created ID for DOES segments. Starts with "d"
    followed by a number, e.g., d1. Used in creating automatic step IDs.

  - key_does: Keyword describing the DOES segment. Used in
    steplist_creator shiny app dropdown menus.

  - subject_singular_does: Description used if subject (WHAT segment in
    front) has been specified as singular (plural_what=0).

  - subject_plural_does: Description used if subject (WHAT segment in
    front) has been specified as plural (plural_what=1).

  - no_subject_does: Description used if no subject (WHAT segment in
    front) has been specified.

  - then_object_does: Indicates if the object for this DOES segment is a
    WHAT segment (0) or a THEN statement (1).

- `where`:

  A list of locations (WHERE segments), where the specified actions take
  place, e.g., in the airways, with the following variables:

  - id_where: Automatically created ID for WHERE segments. Starts with
    "e" followed by a number, e.g., e1. Used in creating automatic step
    IDs.

  - key_where: Keyword describing the WHERE segment. Used in
    steplist_creator shiny app dropdown menus.

  - desc_where: Text used in step descripiton. Please include the
    corresponding preposition, e.g., 'in', 'into', 'on', etc.

- `then`:

  A list of combinations of WHAT, DOES and WHERE segments (THEN
  statements). A THEN statement can contain up to 4 segments: WHAT
  (subject), DOES, WHAT (object), WHERE. Not all 4 of them need to be
  specified. For some DOES segments, the corresponding object is not a
  WHAT segment but a THEN statement (see `then_object_does`). In
  general, all combinations are possible, although only DOES, only
  WHERE, and WHAT WHAT do not make a lot of sense. `then` exists to
  store the THEN statements that are later used in IF and IFNOT
  conditions. It contains the following variables:

  - id_then: Automatically created ID based on segment IDs, e.g., a4,
    a1d5a15e9, d2a3.

  - desc_then: Automatically created description based on segment
    descriptions.

- `module`:

  Modules are groups, into which the steps are sorted, e.g., immune
  system, lung, etc., as it is sometimes of interest to see which groups
  are involved in the sufficient causes. It contains the following
  variables:

  - id_module: Automatically created ID for modules. Starts with "m"
    followed by a number, e.g., m1.

  - key_module: Keyword describing the module.

  - desc_module: Module description.

- `step`:

  Main table of interest and the one further processed to create
  sufficient-component cause models. It contains the following
  variables:

  - id_step: Automatically created step ID based on IDs of included THEN
    statements, e.g., IFd6a10IFNOTd6a18+d1a8THENa11d3a12.

  - desc_step: Automatically created step description based on
    descriptions of included THEN statements.

  - end_step: Indicator variable that describes if this step is at the
    end of a certain sub-mechanism, e.g., symptom x occured.

  - module_step: Module, i.e., group, into which this step has been
    sorted.

  - note_step: Additional notes that are important for future users,
    e.g., if there are conflicting results or if the result is from a
    mouse model.

  - ref_step: References on which this step is based.

- `icc`:

  ICC is short for incompatibel component causes. It contains pairs of
  component causes, i.e., steps without IF or IFNOT condition, that are
  not compatible with each other, i.e., cannot appear in the same
  sufficient cause. It contains the following variables:

  - id_icc: Automatically created ID for ICC pairs. Starts with "i"
    followed by a number, e.g., i1.

  - id1: Step ID of first component cause.

  - id2: Step ID of second component cause.

  - desc1: Step description of first component cause.

  - desc2: Step description of second component cause.

- `outc`:

  A list that contains conditions under which the outcome of interest is
  assumed to occur. Each line might contain one or more THEN statements,
  that have been marked as end steps by setting step\$end_step to 1. If
  more than one THEN statement is selected, they are combined with AND
  logic. All lines in this table are combined with OR logic, i.e., any
  of the specified conditions is assumed to represent outcome
  occurrence. The table contains the following variables:

  - id_outc: Automatically created ID for outcome definitions as a
    combination of the THEN statement IDs connected by '+'.

  - desc_outc: Automatically created description for the outcome
    definitions as a combination of the THEN statement descriptions.

### `epicmodel_steplist_checked` objects

Before using `epicmodel_steplist` object for SCC model creation in
[`create_scc()`](https://forsterepi.github.io/epicmodel/reference/create_scc.md),
they need to be checked for any structures that might make SCC model
creation impossible. Checking is performed by
[`check_steplist()`](https://forsterepi.github.io/epicmodel/reference/check_steplist.md)
and if successful, the returned object is of type
`epicmodel_steplist_checked`. When changing the steplist in the Steplist
Creator `shiny` app or by functions
[`remove_all_modules()`](https://forsterepi.github.io/epicmodel/reference/remove_all_modules.md),
[`remove_na()`](https://forsterepi.github.io/epicmodel/reference/remove_na.md),
or
[`remove_segment()`](https://forsterepi.github.io/epicmodel/reference/remove_segment.md),
the steplist is "un-checked" and returned as class `epicmodel_steplist`.
Apart from that, both classes have similar structure, which can be
validated by `validate_steplist()`.

## Examples

``` r
# Create steplists in the Steplist Creator `shiny` app
if(interactive()){
launch_steplist_creator()
}

# Download the steplist from the `shiny` app
# Load the steplist into R
path <- system.file("extdata", "steplist_rain.rds", package = "epicmodel")
steplist <- readRDS(path)

# new_steplist(), validate_steplist(), and empty_steplist() are used in the `shiny` app
# nonetheless, you can check steplist structures with validate_steplist()
validate_steplist(steplist)
#> ✖ unchecked (please run `check_steplist()` before continuing)
#> WHAT:  7  WHAT segments
#> DOES:  6  DOES segments
#> WHERE:  3  WHERE segments
#> MODULE:  3  modules
#> STEP:  10  STEPs
#> ICC:  0  incompatible component-cause pairs
#> OUTCOME:  1  outcome definition

# print() provides a summary of steplist entries and if it's checked or unchecked
print(steplist)
#> ✖ unchecked (please run `check_steplist()` before continuing)
#> WHAT:  7  WHAT segments
#> DOES:  6  DOES segments
#> WHERE:  3  WHERE segments
#> MODULE:  3  modules
#> STEP:  10  STEPs
#> ICC:  0  incompatible component-cause pairs
#> OUTCOME:  1  outcome definition

# Check steplist before using `summary()` and `plot()`
steplist_checked <- check_steplist(steplist)
#> 
#> ── Checking epicmodel_steplist steplist ────────────────────────────────────────
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
summary(steplist_checked)
#> 
#> ── Outcome Definitions ──
#> 
#> • you get wet
#> 
#> ── Component causes ──
#> 
#> • no vacation
#> • weekday
#> • rain
#> • get groceries
#> 
#> ── Interventions ──
#> 
#> • take vacation
#> • take umbrella
#> • work from home
#> 
#> ── End steps ──
#> 
#> • you get wet
#> 
#> ── Other steps ──
#> 
#> • walk to work
#> • go outside
plot(steplist_checked)
#> $Graph
#> 
#> $Legend
#>    Label   Module           Step
#> 1    CC1 activity    no vacation
#> 2    CC2     fate        weekday
#> 3    CC3  weather           rain
#> 4    CC4 activity  get groceries
#> 5     I1 activity  take vacation
#> 6     I2 activity  take umbrella
#> 7     I3 activity work from home
#> 8     S1 activity   walk to work
#> 9     S2 activity     go outside
#> 10    E1  weather    you get wet
#> 
```
