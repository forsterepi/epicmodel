# Extracting component causes from SCC model

Extracting component causes by sufficient cause from an `epicmodel_scc`
object.

## Usage

``` r
scc_cause_sets(
  scc,
  output = c("id", "desc", "desc_no_start", "all"),
  depends = TRUE,
  unknown = FALSE
)
```

## Arguments

- scc:

  An object of class `epicmodel_scc`.

- output:

  A single element of type character, which determines the type of
  output. Options are "id", "desc", "desc_no_start", and "all". See
  returns-part below for description.

- depends:

  TRUE (default) or FALSE. If FALSE, only includes sufficient causes
  with sc_status "always".

- unknown:

  TRUE or FALSE (default). If TRUE, unknown causes are added to the SCC
  model: every sufficient cause gets an additional individual unknown
  component cause representing additional unknown components; an unknown
  sufficient cause is added to the model consisting of a single unknown
  component cause and representing all unknown sufficient causes.

## Value

A named list but its content depends on parameter "output". The names
correspond to the component cause set IDs, i.e., `cc[[:digit:]]+`.

- id: Returns a named list of character vectors. Each vector contains
  the step IDs of its component causes.

- desc: Returns a named list of character vectors. Each vector contains
  the step descriptions of its component causes.

- desc_no_start: Returns a named list of character vectors. Each vector
  contains the step descriptions of its component causes, but with the
  "Start: " in the beginning removed.

- all: A named list of the three lists above. The names correspond to
  the corresponding option for parameter "output".

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

# Get sets of component causes that form the sufficient causes
scc_cause_sets(scc_model, output = "all")
#> $id
#> $id$cc5
#> [1] "THENa1"   "THENd2a3"
#> 
#> $id$cc14
#> [1] "IFNOTd6a6THENd5a6" "THENa5"            "THENa1"           
#> 
#> 
#> $desc
#> $desc$cc5
#> [1] "Start: rain"          "Start: get groceries"
#> 
#> $desc$cc14
#> [1] "Start: IFNOT take vacation THEN no vacation"
#> [2] "Start: weekday"                             
#> [3] "Start: rain"                                
#> 
#> 
#> $desc_no_start
#> $desc_no_start$cc5
#> [1] "rain"          "get groceries"
#> 
#> $desc_no_start$cc14
#> [1] "no vacation" "weekday"     "rain"       
#> 
#> 
```
