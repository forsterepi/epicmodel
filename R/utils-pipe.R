#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
#' @noRd
NULL

#' Assignment pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%<>\%}} for details.
#'
#' @name %<>%
#' @keywords internal
#' @importFrom magrittr %<>%
#' @usage lhs \%<>\% rhs
#' @param lhs An object which serves both as the initial value and as target.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `lhs <- rhs(lhs)`.
#' @noRd
NULL

# Avoid the devtools::check() NOTE that the . used in piping is a non-declaree variable
utils::globalVariables(".")

# devtools::check() does not realize that packages DT, prompter, shinyalert, shinyjs, shinythemes are used in the shinyapp code
# Strategy suggested in R packages (2e), 11.4.1.1 How to not use a package in Imports
ignore_unused_imports <- function() {
  DT::formatCurrency()
  prompter::use_prompt()
  shinyalert::useShinyalert()
  shinyjs::useShinyjs()
  shinythemes::shinytheme()
}
