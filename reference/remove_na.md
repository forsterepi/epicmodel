# Removing NA in `icc` and `outc`

Remove any entries that only consist of NA from data.frames `icc`
(Incompatible Component Causes) and `outc` (outcome definition) from an
`epicmodel_steplist`.

## Usage

``` r
remove_na(steplist)
```

## Arguments

- steplist:

  An `epicmodel_steplist` or `epicmodel_steplist_checked` object.

## Value

An `epicmodel_steplist` object without entries in data.frame `icc`,
which contain 'NA' in either `id1` or `id2` as well as entries in
data.frame `outc` that contain 'NA' in `id_outc`. If you made any
changes, you need to call
[`check_steplist()`](https://forsterepi.github.io/epicmodel/reference/check_steplist.md)
again.

## Examples

``` r
x <- remove_na(steplist_party)
```
