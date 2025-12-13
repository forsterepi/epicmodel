# Explore effect of prevention

Prevention refers to the avoidance of component causes, i.e., of
elements of sufficient causes. For a given set of component causes,
`prevent()` derives, which of them need to be "removed" in order to
avoid outcome occurrence. Reported are the smallest prevention sets,
i.e., with the fewest component causes.

## Usage

``` r
prevent(scc, causes = NULL, output = c("nice", "table"))
```

## Arguments

- scc:

  An object of class `epicmodel_scc`.

- causes:

  A character vector containing step IDs of component causes. If NULL
  (default), prints a list of all available component causes in the
  console.

- output:

  Either "nice" (default) or "table". If "nice", prints a nicely
  formatted summary in the console. If "table", returns a data.frame
  (described in section "Value" below).

## Value

If `output = "nice"` (default), prints a nicely formatted output in the
console. If `output = "table"`, returns a data.frame with one row for
every prevention set and one column for every component cause provided
in argument `causes`. All cells are either TRUE or FALSE with TRUE
indicating that the corresponding variable needs to be prevented in the
corresponding set, and FALSE indicating that prevention in the
corresponding set is not necessary.

## Details

The following algorithm is used to evaluate the effect of prevention:

- Evaluate if `causes` is sufficient for outcome occurrence. If not,
  report so and stop.

- Derive a list of all combinations of the component causes provided in
  `causes`. The set "all causes present" is not evaluated as it is
  already known to be sufficient. In addition, the set "all causes
  absent", i.e., "all causes prevented" is considered.

- Evaluate sufficiency for every set

- Subset the list of cause sets to the ones, which are `not` sufficient,
  because for them prevention was successful.

- Turn all FALSE to TRUE and all TRUE to FALSE. Now, FALSE indicates
  present and TRUE indicates absent, i.e., prevented.

- Evaluate, which prevention sets are minimal, i.e., the smallest set to
  prevent the outcome.

## Examples

``` r
# Derive SCC model
scc_model <- scc_rain

# Derive prevention sets
prevent(scc_model, causes = c("IFNOTd6a6THENd5a6","THENa5","THENa1","THENd2a3"))
#> ✔ 11/11 | Check if prevention set is minimal
#> ℹ 3/11 prevention sets are minimal
#> 
#> ── Prevention ──────────────────────────────────────────────────────────────────
#> 
#> ── Cause set ──
#> 
#> Individuals with exactly the following component causes:
#> • no vacation
#> • weekday
#> • rain
#> • get groceries
#> 
#> ── Prevention sets ──
#> 
#> Prevent the outcome by preventing any of the following sets:
#> 
#> ── Set  1 
#> • rain
#> 
#> ── Set  2 
#> • weekday
#> • get groceries
#> 
#> ── Set  3 
#> • no vacation
#> • get groceries
#> 
```
