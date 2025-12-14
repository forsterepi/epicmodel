# Changelog

## epicmodel (development version)

## epicmodel 0.2.1

- Update email address

- Update roxygen to 7.3.3

- Use `.internal` of
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html)

- Use [`all()`](https://rdrr.io/r/base/all.html) and
  [`any()`](https://rdrr.io/r/base/any.html)

- Replace `label.size` with `linewidth` in a call to
  [`ggplot2::geom_label()`](https://ggplot2.tidyverse.org/reference/geom_text.html)

## epicmodel 0.2.0

CRAN release: 2024-12-11

- [`prevent()`](https://forsterepi.github.io/epicmodel/reference/prevent.md)
  now enables investigation of prevention sets

- [`necessary_causes()`](https://forsterepi.github.io/epicmodel/reference/necessary_causes.md)
  now enables extraction of necessary causes from `epicmodel_scc`
  objects

- Steplist Creator Shiny App can now be used on smaller screens

- Fixed a bug that causes the steplist creator shiny app to fail
  ([\#2](https://github.com/forsterepi/epicmodel/issues/2))

- Fixed a bug in function intervene()

## epicmodel 0.1.1

CRAN release: 2024-11-08

- Initial release on CRAN.
