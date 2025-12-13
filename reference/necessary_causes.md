# Extract necessary causes

Necessary causes are component causes, which are part of every
sufficient cause and, therefore, have to be present in order for the
outcome to occur.

## Usage

``` r
necessary_causes(scc, output = c("id", "desc", "desc_no_start"))
```

## Arguments

- scc:

  An object of class `epicmodel_scc`.

- output:

  A single element of type character determining the type of output.
  Either `id` (default), `desc`, or `desc_no_start`. See section "Value"
  below for a description of the output.

## Value

A character vector containing all necessary causes. Depending on the
value of `output`, the vector contains either step IDs
(`output = "id"`), step descriptions (`output = "desc"`), or step
descriptions but with the "Start: " in the beginning removed
(`output = "desc_no_start"`).

## Examples

``` r
necessary_causes(scc_rain)
#> [1] "THENa1"
```
