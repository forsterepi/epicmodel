# Determine standardized effect size of component causes

SCC models teach us that effect strength, e.g., a risk ratio, is no
natural constant but depends on the prevalence of component causes and,
therefore, differs between populations. However, even without any
population, this function derives effect sizes for every component cause
by comparing how many sets of component causes with and without a
certain cause are sufficient to cause the outcome of interest.

## Usage

``` r
effect_size(scc, depends = TRUE, output = c("nice", "table"))
```

## Arguments

- scc:

  An object of class `epicmodel_scc`.

- depends:

  TRUE (default) or FALSE. If FALSE, only includes sufficient causes
  with suffciency status "always".

- output:

  A single element of type character, either "nice" (default) or
  "table". If "table", returns a data.frame. If "nice", a nicely
  formated output is printed in the console.

## Value

Either a dataframe (`output` = "table") with one row for every component
cause and with variables `id` (step ID), `desc` (step description),
`suff_true`, `suff_false`, `num_combos_true`, `num_combos_false`, and
`ratio`, or a nicely formated output in the console (`output` = "nice").
See Details for more information.

## Details

The following algorithm is used to derive effect sizes from SCC models:

- The effect size is derived for one specific component cause. The
  following steps are repeated for all of them.

- Get all potential combinations of component causes

- Remove combinations that contain incompatible component causes (ICC),
  as specified in the steplist

- Split the set of possible combinations of component causes into two
  parts: Sets, in which the component cause of interest is present &
  sets, in which the component cause of interest is absent. The numbers
  are recorded and returned in the output table (output = "table") as
  variables `num_combos_true` (cause is present) and `num_combos_false`
  (cause is absent). If there are no incompatible component causes
  (ICC), both values should be the same.

- Check for all possible combinations of component causes, if they are
  sufficient for the outcome to occur. The number of sufficient
  combinations are counted separately for combinations with the
  component cause of interest present and combinations with the
  component cause of interest absent. The numbers are recorded and
  returned in the output table (output = "table") as variables
  `suff_true` (cause is present) and `suff_false` (cause is absent).

- A ratio is calculated using the following formula:
  `(suff_true / num_combos_true) / (suff_false / num_combos_false)`. In
  the output table (output = "table"), this value is stored in variable
  `ratio`. In the nice output (output = "nice"), it is reported in the
  column `Impact`, which shows:
  `ratio [suff_true/num_combos_true vs. suff_false/num_combos_false]`

- There are two special cases when calculating the `ratio`. When
  `suff_true > 0` but `suff_false == 0`, the outcome only occurs if the
  corresponding component cause is present. The `ratio` then gets value
  `necessary`. When `suff_true == 0` and `suff_false == 0`, the `ratio`
  gets value `not a cause`.

## Examples

``` r
# Create some SCC model
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
scc_model <- create_scc(steplist_checked)
#> 
#> ── Create SCC Model ──
#> 
#> ✔ 15/15 | Check if set of component causes is sufficient
#> ✔ 5/5 | Check if sufficiency dependends on IFNOT conditions
#> ✔ 5/5 | Check if sufficient cause is minimal
#> ℹ 2/5 sufficient causes are minimal

# Use the SCC model in effect_size()
effect_size(scc_model)
#> ✔ 4/4 | Check impact of every component cause
#>                        Component Cause                  Impact
#> 1                                 rain necessary [5/8 vs. 0/8]
#> 2                        get groceries      4.00 [4/8 vs. 1/8]
#> 3 IFNOT take vacation THEN no vacation      1.50 [3/8 vs. 2/8]
#> 4                              weekday      1.50 [3/8 vs. 2/8]
```
