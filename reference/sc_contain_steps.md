# Do steps appear in sufficient causes?

Extracts from a SCC model, if certain steps are part of the mechanism of
sufficient causes. If you want a list of all steps, ignore argument
`steps`.

## Usage

``` r
sc_contain_steps(scc, steps = NULL, output = c("nice", "table"))
```

## Arguments

- scc:

  An object of class `epicmodel_scc`.

- steps:

  A character vector containing step IDs. IF NULL (default), provides a
  list of all steps.

- output:

  A single element of type character, either "nice" (default) or
  "table". If "table", returns a list (or data.frame if steps = NULL).
  If "nice", a nicely formated output is printed in the console.

## Value

Either a list (`output` = "table") with length equal to the number of
sufficient causes and each element being a named vector of TRUE/FALSE
with the variables in `steps` as names and TRUE indicating that the step
appears in the corresponding sufficient cause, or a nicely formated
output in the console (`output` = "nice"). If steps = NULL and output =
"table", returns a data.frame, which contains variables `id_step` and
`desc_step` from the `epicmodel_steplist_checked` data.frame `step`.

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

# Check if one or more steps are part of the mechanism for each sufficient cause
sc_contain_steps(scc_model, c("THENa1","THENa5"))
#> 
#> ── SC 1 ──
#> 
#> Component causes:
#> • rain
#> • get groceries
#> 
#> ✔ SC1 contains step 'Start: rain' (THENa1)
#> ✖ SC1 does not contain step 'Start: weekday' (THENa5)
#> 
#> ── SC 2 ──
#> 
#> Component causes:
#> • no vacation
#> • weekday
#> • rain
#> 
#> ✔ SC2 contains step 'Start: rain' (THENa1)
#> ✔ SC2 contains step 'Start: weekday' (THENa5)
```
