# Remove all modules

Removes all entries in data.frame `module` from an `epicmodel_steplist`
object. Also turns all values of variable `module_step` in data.frame
`step` from an `epicmodel_steplist` to empty strings.

## Usage

``` r
remove_all_modules(steplist)
```

## Arguments

- steplist:

  An `epicmodel_steplist` or `epicmodel_steplist_checked` object.

## Value

An `epicmodel_steplist` object with empty data.frame `module` and empty
strings in variable `module_step` in data.frame `step`. When continuing
with this steplist, SCC models cannot be inspected by module. If you
made any changes, you need to call
[`check_steplist()`](https://forsterepi.github.io/epicmodel/reference/check_steplist.md)
again.

## Examples

``` r
x <- remove_all_modules(steplist_party)
```
