# Check `epicmodel_steplist` class objects

Check if `epicmodel_steplist` class objects fulfill the conditions for
being inputed in
[`create_scc()`](https://forsterepi.github.io/epicmodel/reference/create_scc.md).

## Usage

``` r
check_steplist(steplist)
```

## Arguments

- steplist:

  An object of class `epicmodel_steplist`.

## Value

Prints information about successful and unsuccessful checks in the
console. Returns the input steplist. If checks were successful, returns
a steplist of class `epicmodel_steplist_checked` that can be used in
building SCC models.

## Details

The following checks are conducted:

### Errors

- Correct ID format in WHAT segments

- No duplicated IDs in WHAT segments

- Correct ID format in DOES segments

- No duplicated IDs in DOES segments

- Correct ID format in WHERE segments

- No duplicated IDs in WHERE segments

- Correct ID format in Modules

- No duplicated IDs in Modules

- Correct ID format in ICC

- No duplicated IDs in ICC

- All WHAT segments used in data.frame `step` must be listed in
  data.frame `what`

- All DOES segments used in data.frame `step` must be listed in
  data.frame `does`

- All WHERE segments used in data.frame `step` must be listed in
  data.frame `where`

- All modules used in data.frame `step` must be listed in data.frame
  `modules`

- Either all steps or no steps have modules specified in data.frame
  `step`

- All step IDs used in ICC definition must be specified in data.frame
  `step`

- Starting steps, i.e., steps without IF condition, must not have
  `end_step == 1` in data.frame `step`

- A steplist must contain component causes

- In case there are two steps with identical THEN statements, they
  cannot have both `end_step == 1` and `end_step == 0` in data.frame
  `step`

- THEN statements used in IF/IFNOT conditions must be available for
  chaining, i.e., there must be a step with this statement as its THEN
  part and this step must not be defined as end step

- For all steps, their THEN statement must be available in data.frame
  `then`

- A step must not have identical IF and IFNOT conditions

- A step’s THEN statement must not be part of its own IF/IFNOT condition

- All steps used in the outcome definition must be in data.frame `step`
  with `end_step == 1`

### Warnings

- No duplicated keywords in WHAT segments

- No duplicated keywords in DOES segments

- No duplicated keywords in WHERE segments

- No duplicated keywords in Modules

- All WHAT segments in data.frame `what` should be used in data.frame
  `step`

- All DOES segments in data.frame `does` should be used in data.frame
  `step`

- All WHERE segments in data.frame `where` should be used in data.frame
  `step`

- All modules in data.frame `modules` should be used in data.frame
  `step`

- All steps should have references

- There should not be any steps with identical THEN statements

- All steps with `end_step == 1`in data.frame `step` should be used in
  the outcome definition

- Outcome definitions should not be contained in each other, e.g., for
  outcome definition `(A and B) or (A and B and C)`, `A and B` is
  contained, i.e., a subset of, `A and B and C`, which makes
  `A and B and C` redundant

## Examples

``` r
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
```
