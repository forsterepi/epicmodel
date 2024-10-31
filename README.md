
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epicmodel

<!-- badges: start -->

[![R-CMD-check](https://github.com/forsterepi/epicmodel/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/forsterepi/epicmodel/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Overview

`epicmodel` is short for **“Causal Modeling in Epidemiology”** and wants
to offer all necessary tools for a causal modeling workflow in R for
epidemiologists. Causal modeling describes a structured process of
making causal assumptions based on which an epidemiological study is
conduted and its results are interpreted. We are always making causal
assumptions, at least implicitly. Causal modeling is about doing so
explicitly. Did you ever wonder what to measure, how to define your
variables, or how to model your outcome of interest? If yes, chances are
you need to think about your causal model in more detail.

Causal models are created by making causal assumptions (i.e., that
variable A causes variable B) within a **causal modeling framework**.
The current version of `epicmodel` focuses on one of these frameworks,
sufficient-component cause (SCC) models, and offers a way to create them
using R. SCC models describe, which sets of causes are in combination
sufficient for the outcome of interest to occur.

The package documentation contains many terms with very specific
meanings in the the context of this package. Check the **glossary** for
an overview: `vignette("glossary")`.

## Usage

Creating SCC models follows a three-step workflow (see
`vignette("epicmodel")` for an overview):

1.  Create the input for SCC model creation, the so called **steplist**,
    using the built-in shiny app. See `vignette("steplist")` for
    details.
2.  Let `epicmodel` create the **SCC model** from the steplist.
3.  Use the SCC model, e.g., for:

- Estimating **standardized effect size**
- Investigating the effect of **interventions** (see
  `vignette("interventions")` for details)
- Inspecting the **mechanisms** behind sufficient cause (see
  `vignette("mechanisms")` for details)
- Transforming the SCC model to a directed acyclic graph (**DAG**) (see
  `vignette("dag")` for details)

## Installation

For the latest release:

``` r
install.packages("epicmodel")
```

For the development version:

``` r
# install.packages("devtools")
devtools::install_github("forsterepi/epicmodel")
```
