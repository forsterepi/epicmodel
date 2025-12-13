# Transform SCC to DAG

Creates an object of class `dagitty` (`dagitty` package) from a SCC
model, following VanderWeele and Robins (2007).

## Usage

``` r
scc_to_dag(scc, unknown = TRUE)
```

## Arguments

- scc:

  An object of class `epicmodel_scc`.

- unknown:

  TRUE (default) or FALSE. If TRUE, unknown causes are added to the SCC
  model: every sufficient cause gets an additional individual unknown
  component cause representing additional unknown components; an unknown
  sufficient cause is added to the model consisting of a single unknown
  component cause and representing all unknown sufficient causes.

## Value

A list of length 2 containing an object of class `dagitty` (named `dag`)
and a data.frame containing the information, which label in the DAG
corresponds to which component cause (named `legend`).

## References

VanderWeele TJ, Robins JM (2007): Directed acyclic graphs, sufficient
causes, and the properties of conditioning on a common effect. American
Journal of Epidemiology 166 (9): 1096–1104.

## See also

- [`dagitty::dagitty()`](https://rdrr.io/pkg/dagitty/man/dagitty.html)

- [`SCC models`](https://forsterepi.github.io/epicmodel/reference/epicmodel_scc.md)
  for more information on unknown causes and SCC models in general

- [`plot_dag()`](https://forsterepi.github.io/epicmodel/reference/plot_dag.md)
  to create a `ggplot` object from `dagitty` model code

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

# Transform it into a DAG
scc_to_dag(scc_model)
#> $dag
#> dag {
#> CC1 [pos="1.000,0.250"]
#> CC2 [pos="1.000,0.750"]
#> CC3 [pos="1.000,1.250"]
#> CC4 [pos="1.000,1.750"]
#> O [outcome,pos="4.000,1.000"]
#> SC1 [pos="3.000,0.000"]
#> SC2 [pos="3.000,1.000"]
#> USC [pos="3.000,2.000"]
#> U_SC1 [pos="2.000,0.000"]
#> U_SC2 [pos="2.000,1.000"]
#> U_USC [pos="2.000,2.000"]
#> CC1 -> SC1
#> CC1 -> SC2
#> CC2 -> SC1
#> CC3 -> SC2
#> CC4 -> SC2
#> SC1 -> O
#> SC2 -> O
#> USC -> O
#> U_SC1 -> SC1
#> U_SC2 -> SC2
#> U_USC -> USC
#> }
#> 
#> $legend
#>            desc label
#> 1          rain   CC1
#> 2 get groceries   CC2
#> 3   no vacation   CC3
#> 4       weekday   CC4
#> 
```
