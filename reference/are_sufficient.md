# Check if a certain set of component causes is suffcient

Provide a SCC model and a set of component causes and evaluate if the
provided set of causes fulfills any sufficient cause, i.e., is
sufficient for the outcome to occur based on the provided SCC model.
Fulfilling a sufficient cause means that all component causes of a
certain sufficient cause are in the provided set of causes. Unknown
causes are ignored by this function.

## Usage

``` r
are_sufficient(scc, causes = NULL, type = c("status", "binary"))
```

## Arguments

- scc:

  An object of class `epicmodel_scc`.

- causes:

  NULL (default) or a character vector containing IDs of a set of
  component causes. If NULL, prints a list of all available component
  causes.

- type:

  Either "status" (default) or "binary". If "status", returns one of
  "always", "depends", "never". If "binary", returns TRUE or FALSE.

## Value

For `type = "binary`, returns TRUE if all component causes for at least
one sufficient cause are in `causes` and FALSE otherwise. For
`type = status`, returns "always" if at least one sufficient cause with
sufficiency status "always" is fulfilled. If not, returns "depends" if
at least one sufficient cause with sufficiency status "depends" or
"depends (potential order implausibilities)" is fulfilled. If no
sufficient cause is fulfilled, returns "never".

## Details

Depending on the value of `type`, the following values are possible:

- `type = "status"`: If the provided set of `causes` contains all
  component causes of a sufficient cause with status "always", returns
  "always". If the provided set of `causes` only fulfills sufficient
  cause with status "depends" or "depends (potential order
  implausibilities)", returns "depends". If no sufficient causes are
  fulfilled, returns "never".

- `type = "binary"`: If the returned status would have been "always" or
  "depends", TRUE is returned. If the returned status would have been
  "never", returns FALSE.

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

# Check sufficiency for a certain set of component causes
are_sufficient(scc_model, c("THENa1","THENa5"), type = "status")
#> [1] "never"
are_sufficient(scc_model, c("THENa1","THENa5"), type = "binary")
#> [1] FALSE
```
