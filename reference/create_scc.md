# Creating SCC models

Creates a sufficient-components cause (SCC) model from a steplist, which
is a list of IF/THEN statements describing the causal mechanism behind
an outcome of interest. The steplist needs to meet certain structural
requirements. Therefore, for steplist creation, use the Steplist Creator
`shiny` app launched by
[`launch_steplist_creator()`](https://forsterepi.github.io/epicmodel/reference/launch_steplist_creator.md).

## Usage

``` r
create_scc(steplist)
```

## Arguments

- steplist:

  An object of class `epicmodel_steplist_checked`.

## Value

An object of class `epicmodel_scc`. If no sufficient causes are found,
no object is returned but instead a corresponding message is displayed
in the console.

## Details

The following algorithm is used to create a sufficient-component cause
(SCC) model from a steplist.

- Check inputs: The steplist needs to be checked by
  [`check_steplist()`](https://forsterepi.github.io/epicmodel/reference/check_steplist.md)
  before input

- Are modules used: Evaluate if the steplist contains modules

- Process steplist: Process steplist and outcome definition so that they
  can be used by the procedure

- Get all combinations of component causes in the steplist: Component
  causes are steps, which themselves have no IF condition but appear in
  IF conditions of other steps (and maybe additionally in IFNOT
  conditions). Interventions are not considered to be component causes.
  Interventions are as well steps without IF condition, but they only
  appear in IFNOT conditions of other steps. Invalid combinations of
  component causes as specified in the ICC part of the steplist are
  excluded, as well as every component cause being absent.

- Check sufficiency: Sufficiency is checked for every combination of
  component causes. First, based on a specific set of component causes,
  it is derived, which steps can be caused by this set, i.e., which IF
  conditions are fulfilled. For this, a current set of included steps is
  defined, which in the beginning includes only the corresponding set of
  component causes. Then, it is iteratively checked, for which other
  steps with IF condition (i.e., excluding non-selected component causes
  and interventions) this IF condition is fulfilled. These steps are
  added to the current set of included steps and the process is repeated
  until for no new steps the IF condition is fulfilled. Second, this
  final list of steps is compared against the outcome definitions. If it
  is fulfilled, the set of component causes is sufficient.

- Check IFNOT conditions: Please note that IFNOT conditions were ignored
  up to this point. Now, all sets of component causes that were found to
  be sufficient previously, are re-checked for IFNOT conditions. First,
  it is checked if there are any IFNOT conditions in the final list of
  steps derived above and if those are fulfilled based only on the other
  steps in this list. If no, checking is complete and the corresponding
  set of component causes is always sufficient. If yes, further checking
  is required. In these cases, sufficiency depends on the order in which
  individual steps occur. In principle, a step with both IF and IFNOT
  conditions fulfilled, occurs if the IF condition is fulfilled before
  the IFNOT condition, similar to how I do not care if a door is closed
  if I already went through it when it was still open. Please note that
  this approach extends SCC models by an additional time component.
  Sufficiency is therefore re-checked for all possible sequences of IF
  and IFNOT conditions of all steps that include IFNOT conditions that
  can be fulfilled by the final set of steps. It is possible to have
  component causes with IFNOT conditions. Since they do not have an IF
  condition, the THEN statement is used instead. For every sequence, it
  is evaluated if the IF (or THEN for component causes) occurs before or
  after the IFNOT. If IF/THEN occur after the corresponding IFNOT, this
  step is removed from the final list of steps. Sufficieny is now
  re-checked based on the updated list. If some orderings do not fulfill
  the outcome definition, the sufficiency status of the corresponding
  set of component causes is changed to "depends", as it depends on the
  sequence of events. Please note that currently, all sequences are
  checked even though some of them might be implausible, e.g., when two
  steps with IFNOT conditions are chained together. In this case, there
  will be a warning displayed, but the user ultimately needs to check
  plausibility of the sequence of events.

- Minimize: Sufficient causes must be minimal by definition, i.e., every
  component cause must be necessary within its sufficient cause, i.e.,
  the absence of one component cause of a sufficient set means that the
  outcome does not occur anymore. Therefore, the list of sufficient
  (both always and depends) sets of component causes is reduced to
  minimal ones.

- Add unknown causes: It is possible/likely that unknown causes, both
  component causes and sufficient causes, are not part of the model yet.
  Therefore, every sufficient cause gets an additional individual (i.e.,
  a different one for each sufficient cause) unknown component cause
  representing additional unknown components, and one unknown sufficient
  cause is added to the model consisting of a single unknown component
  cause and representing all unknown sufficient causes. If relevant, the
  user can decide in functions with the SCC model as input if unknown
  causes should be included or not.

- Output preparation: Combines all outputs to an object of class
  `epicmodel_scc` for further analysis.

## References

Rothman KJ (1976): Causes. American Journal of Epidemiology 104 (6):
587–592.

## See also

- [`SCC models`](https://forsterepi.github.io/epicmodel/reference/epicmodel_scc.md)
  for information on `epicmodel_scc` objects

- [`Steplist`](https://forsterepi.github.io/epicmodel/reference/epicmodel_steplist.md)
  for information on `epicmodel_steplist` objects

## Examples

``` r
# First, create a steplist in the shiny app
# Launch the app with launch_steplist_creator()
# Then load your steplist using readRDS()
# In this example we use the built-in steplist_rain

# Check the steplist before running create_scc()
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

# Use the checked steplist in create_scc()
scc_model <- create_scc(steplist_checked)
#> 
#> ── Create SCC Model ──
#> 
#> ✔ 15/15 | Check if set of component causes is sufficient
#> ✔ 5/5 | Check if sufficiency dependends on IFNOT conditions
#> ✔ 5/5 | Check if sufficient cause is minimal
#> ℹ 2/5 sufficient causes are minimal
```
