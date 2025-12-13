# Explore effect of interventions

Interventions are steps without IF condition (start steps) that only
appear in other IFNOT conditions, i.e., that can only prevent steps but
not cause them. Interventions are not considered when creating SCC
models using
[`create_scc()`](https://forsterepi.github.io/epicmodel/reference/create_scc.md).
`intervene()` evaluates their impact in two directions: 1) which
sufficient causes can be prevented by certain (sets of) interventions
and 2) which set of interventions is at least needed to prevent the
outcome in an individual with a given set of component causes.

## Usage

``` r
intervene(scc, causes = NULL, intervention = NULL, output = c("nice", "table"))
```

## Arguments

- scc:

  An object of class `epicmodel_scc`.

- causes:

  A character vector containing step IDs of component causes. If "all",
  investigates all sufficient causes, i.e., all minimally sufficient
  sets of component causes. If NULL (default), prints a list of all
  available component causes in the console. If a set of step IDs is
  specified, only the specified set is investigated.

- intervention:

  A character vector containing step IDs of interventions. If "all",
  investigates all possible combinations of available interventions. If
  NULL (default), prints a list of all available interventions in the
  console. If a set of step IDs is specified, investigates all possible
  combinations of the specified interventions.

- output:

  Either "nice" (default) or "table". If "nice", prints a nicely
  formatted summary in the console. If "table", returns a list of
  several elements described in detail in section "Value" below.

## Value

### Output

If `output = "nice"` (default), prints a nicely formatted output in the
console. If `output = "table"`, returns a list with the following
elements:

- `cause_set`:

  A list of character vectors with one element for every investigated
  set of component causes. The character vectors contain the step IDs of
  the component causes that are part of the corresponding set. Sets are
  named in a format similar to cc1, cc2, etc.

- `intv`:

  A list of character vectors with one element for every investigated
  set of interventions. The character vectors contain the step IDs of
  the interventions that are part of the corresponding set. Sets are
  named as intv1, intv2, etc.

- `status`:

  A data.frame with one row per set of component causes and one column
  per set of intervention. In addition, contains one column representing
  no interventions (`intv0`). Each cell contains the sufficiency status
  of the corresponding set of component causes when the corresponding
  set of interventions is applied. Possible values are "always",
  "depends", and "never". See below for an interpretation.

- `minimal`:

  A data.frame with one row per set of component causes and one column
  per set of intervention. Each cell is either TRUE or FALSE indicating
  if the set of interventions is minimal. For non-minimal sets of
  interventions, a smaller set which is contained within the
  corresponding set exists and has the same preventive power. Minimality
  is defined separately for every set of component causes. If both the
  larger non-minimal and the smaller minimal set sometimes prevent the
  outcome (status "depends" in `status` (see above)), the non-minimal
  set might actually prevent more sufficient orders of occurrence than
  the minimal set. In this case, please inspect and compare element
  `order` (see next), for all minimal and non-minimal sets of
  interventions with status "depends".

- `order`:

  A 2-level list, i.e., a list with one element per intervention set,
  for which each element is another list with one element per evaluated
  set of component causes. Each intervention/component causes
  combination contains a data.frame, similar to the data.frames in the
  `sc_order` element of `epicmodel_scc` objects, if the corresponding
  status is "depends", or is NA otherwise (for "always" or "never"). The
  data.frames contain two columns, which are called "order" and "suff"
  (short for "sufficient"), and one row for every order of occurrence.
  The order of occurrence is summarized in "order" (as character), while
  "suff" is either TRUE or FALSE indicating if the corresponding order
  of occurrence is sufficient, i.e., leads to the outcome, or not.
  Please note that the prevented orders of occurrence have
  `suff == FALSE`.

### How to interpret `status`

If the sufficiency status for a certain intervention in column `intv0`
is `always`, the three sufficiency status options for a certain
intervention have the following interpretations:

- `always`: The corresponding set of inteventions never prevents the
  outcome, because after applying the intervention, the corresponding
  set of component causes is still always sufficient.

- `depends`: The corresponding set of interventions sometimes prevents
  the outcome, because after applying the intervention, sufficiency for
  the corresponding set of component causes depends on the order of
  occurrence.

- `never`: The corresponding set of interventions always prevents the
  outcome, because after applying the intervention, the corresponding
  set of component causes is never sufficient.

If the sufficiency status for a certain intervention in column `intv0`
is `depends`, the sufficiency status options for a certain intervention
have the following interpretations:

- `depends`: The corresponding set of interventions sometimes or never
  prevents the outcome, because after applying the intervention,
  sufficiency for the corresponding set of component causes depends on
  the order of occurrence. Further inspection and comparison of
  sufficient orders of occurrence is necessary to determine if the
  intervention actually prevents anything.

- `never`: The corresponding set of interventions always prevents the
  outcome, because after applying the intervention, the corresponding
  set of component causes is never sufficient.

If the sufficiency status for a certain intervention in column `intv0`
is `never`, no intervention is necessary, because the corresponding set
of component causes is never sufficient.

## Details

The following algorithm is used to evaluate the effect of interventions:

- Derive the list of intervention sets to evaluate

- Derive the list of sets of component causes to evaluate

- Evaluate sufficiency without intervention for every set of component
  causes

- Evaluate sufficiency for every combination of intervention set and set
  of componen causes: First, check which steps are prevented by the
  corresponding set of interventions, i.e., for which steps the IFNOT
  condition is fulfilled by the intervention set. These steps are
  removed from the list of available steps. Second, evaluate sufficiency
  based on the remaining steps similar to
  [`create_scc()`](https://forsterepi.github.io/epicmodel/reference/create_scc.md)
  (Check sufficiency & Check IFNOT conditions).

- Evaluate, which intervention sets are minimal, i.e., at least
  necessary to prevent the outcome

## Examples

``` r
# Derive SCC model
scc_model <- scc_rain

# Inspect the effect of interventions
intervene(scc_model, causes = "all", intervention = "all")
#> 
#> ── Intervention ────────────────────────────────────────────────────────────────
#> 
#> ── Cause Set 1 ──
#> 
#> • rain
#> • get groceries
#> 
#> Status without intervention
#> ✔ Always sufficient
#> 
#> Status with intervention
#> ✔ Complete prevention by the following minimal intervention sets
#> 
#> ── Intervention Set 1 
#> • take umbrella
#> 
#> ── Cause Set 2 ──
#> 
#> • no vacation
#> • weekday
#> • rain
#> 
#> Status without intervention
#> ✔ Always sufficient
#> 
#> Status with intervention
#> ✔ Complete prevention by the following minimal intervention sets
#> 
#> ── Intervention Set 1 
#> • work from home
#> 
#> ── Intervention Set 2 
#> • take umbrella
#> 
#> ── Intervention Set 3 
#> • take vacation
intv <- intervene(scc_model, causes = "all", intervention = "all", output = "table")
```
