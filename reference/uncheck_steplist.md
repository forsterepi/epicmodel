# Unchecking `epicmodel_steplist` objects

Putting a checked `epicmodel_steplist` back to an unchecked status.

## Usage

``` r
uncheck_steplist(steplist)
```

## Arguments

- steplist:

  An `epicmodel_steplist` or `epicmodel_steplist_checked` object.

## Value

An object of class `epicmodel_steplist`.

## Examples

``` r
x <- uncheck_steplist(scc_rain$steplist)
```
