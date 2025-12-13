# Remove segments

Removes individual entries from data.frames `what`, `does`, `where`,
`module`, or `icc`.

## Usage

``` r
remove_segment(steplist, id)
```

## Arguments

- steplist:

  An `epicmodel_steplist` or `epicmodel_steplist_checked` object.

- id:

  A single non-missing element of type character describing the ID of
  the entry you want deleted.

## Value

An `epicmodel_steplist` class object. If you made any changes, you need
to call
[`check_steplist()`](https://forsterepi.github.io/epicmodel/reference/check_steplist.md)
again.

## Examples

``` r
steplist_party <- remove_segment(steplist_party, "d4")
```
