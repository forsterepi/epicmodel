# Show all steps of a SCC model

Prints all steps that are part of a sufficient-component cause model.
The function wraps
[`sc_contain_steps()`](https://forsterepi.github.io/epicmodel/reference/sc_contain_steps.md)
with `steps = NULL`.

## Usage

``` r
show_steps(scc, output = c("nice", "table"))
```

## Arguments

- scc:

  An object of class `epicmodel_scc`.

- output:

  A single element of type character, either "nice" (default) or
  "table". If "table", returns a data.frame. If "nice", a nicely
  formated output is printed in the console.

## Value

Either a data.frame (`output` = "table") with variables `id_step` (step
ID) and `desc_step` (step description) and one row for every step in the
model, i.e., from the `epicmodel_steplist_checked` data.frame `step`, or
a nicely formated output in the console (`output` = "nice").

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

# Show all steps
show_steps(scc_model)
#> • THENd6a6: Start: take vacation
#> • IFNOTd6a6THENd5a6: Start: IFNOT take vacation THEN no vacation
#> • THENa5: Start: weekday
#> • THENa1: Start: rain
#> • THENd6a7: Start: take umbrella
#> • THENd4e1: Start: work from home
#> • THENd2a3: Start: get groceries
#> • IFd5a6+a5IFNOTd4e1THENd3e3: IF no vacation and weekday and IFNOT work from
#> home THEN walk to work
#> • IF(d2a3)or(d3e3)THENd1e2: IF get groceries or walk to work THEN go outside
#> • IFd1e2+a1IFNOTd6a7THENa8d2a2: End: IF go outside and rain and IFNOT take
#> umbrella THEN you get wet
```
