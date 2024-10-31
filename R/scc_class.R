#' SCC model objects
#'
#' The S3 class `epicmodel_scc` is used to store information on sufficient-component cause (SCC) models created by [create_scc()].
#'
#' @details
#'  ## `epicmodel_scc` objects
#'  `epicmodel_scc` objects are lists containing 10 elements. These elements are described below:
#'  \describe{
#'  \item{`sc_cc`}{A data.frame with one column for every component cause and one row for every sufficient cause. Colnames are the step IDs from
#'  the corresponding steplist. Rownames are sufficient cause IDs (see below). Each cell contains either TRUE or FALSE indicating if the component
#'  cause in the column is part of a set of component causes described by the row.}
#'
#'  \item{`sc_status`}{A named character vector with one element for every sufficient cause. The names are sufficient cause IDs (see below). The
#'  elements contain the status of the sufficient cause (see below). Here, only "always", "depends", and "depends (potential order
#'  implausibilities)" appear.}
#'
#'  \item{`sc_steps`}{A list of character vectors with one list element for every sufficient cause. The list is named using sufficient cause IDs
#'  (see below). Every character vector contains the step IDs of all steps that are part of the corresponding sufficient cause, i.e., that can be
#'  caused by the corresponding set of component causes.}
#'
#'  \item{`sc_order`}{A list with one list element for every sufficient cause. The list is named using sufficient cause IDs (see below). List
#'  elements are either NA (if a sufficient cause's status is "always") or a data.frame (if a sufficient cause's status is "depends" or "depends
#'  (potential order implausibilities)". Data.frames contain two columns, which are called "order" and "suff" (short for "sufficient"), and one row
#'  for every order of occurrence. The order of occurrence is summarized in "order" (as character), while "suff" is either TRUE or FALSE indicating
#'  if the corresponding order of occurrence is sufficient, i.e., leads to the outcome, or not.}
#'
#'  \item{`sc_implausibilities`}{A named vector of TRUE and FALSE with length equal to the number of sufficient causes. The names are sufficient
#'  cause IDs (see below). Is TRUE if for the corresponding sufficient cause there are potential order implausibilities, i.e., if its status is
#'  "depends (potential order implausibilities)", and is FALSE otherwise.}
#'
#'  \item{`sc_implausibilities_detail`}{A list with one list element for every sufficient cause. The list is named using sufficient cause IDs
#'  (see below). List elements are either NA (if the corresponding element in `sc_implausibilities` is FALSE) or a character vector (if the
#'  corresponding element in `sc_implausibilities` is TRUE) with the THEN statements of the steps that might be involved in implausible orders of
#'  occurrence.}
#'
#'  \item{`sc_use_modules`}{Either TRUE or FALSE indicating if modules have been specified in the steplist.}
#'
#'  \item{`unknown_cc`}{Similar to `sc_cc` but includes unknown component causes and an unknown sufficient cause (see "Unknown causes" below).
#'  It therefore additionally contains:
#'  * one column to the right for every sufficient cause with name "U`rownumber`" (U1, U2, etc.) and all values equal to FALSE appart from row
#'    `rownumber`, which is TRUE
#'  * one additional column to the right with name "USC" and all values equal to FALSE for all sufficient causes
#'  * one additional row with name "cc0" and all values equal to FALSE apart from column "USC", which is TRUE}
#'
#'  \item{`unknown_status`}{Similar to `sc_status` but has one additional element with value "unknown" and name "cc0" (see "Unknown causes" below).}
#'
#'  \item{`steplist`}{The object of class `epicmodel_steplist_checked` that has been the input to function [create_scc()], from which the
#'  `epicmodel_scc` object has been created.}
#'  }
#'  ## Other details:
#'  \describe{
#'  \item{`Sufficient cause IDs`}{[create_scc()] checks every combination of component causes for sufficiency. Every combination is assigned an
#'  ID of the format "cc`number`" (cc1, cc2, etc.). `epicmodel_scc` only contains information about minimally sufficient combinations of component
#'  causes, but the initial IDs are kept. The IDs are used throughout the different elements of `epicmodel_scc` to link information that belongs to
#'  the same sufficient cause. The unknown sufficient cause used in elements `unknown_cc` and `unknown_status` has ID cc0.}
#'
#'  \item{`Unknown causes`}{Since many causes might be unknown, it is reasonable for some applications to include these unknown causes in a SCC
#'  model (see, e.g., Rothman et al. (2008)). They are also useful to remind us of our limited knowledge. In a suffcient-component cause model,
#'  unknown causes come in two flavors:
#'  * `Unknown component causes`: These are additional component causes within a sufficient cause, which are necessary for sufficiency. Please note
#'    that each sufficient cause has its own set of unknown component causes. In `unknown_cc`, unknown component causes are called U1, U2, etc.
#'  * `Unknown sufficient causes`: There might be unknown mechanisms that lead to outcome occurrence. These sufficient causes are summarized in one
#'    additional sufficient cause, which has only a single component cause called `USC` in `unknown_cc`. This set of component causes has
#'    sufficient cause ID `cc0`.
#'
#'  Please note that in [plot_dag()] an ellipse represents a **determinative set** of sufficient causes, as suggested and defined by VanderWeele &
#'  Robins (2007). A determinative set contains all sufficient causes and, therefore, in most cases, an unknown sufficient cause is necessary to
#'  at least achieve a theoretical determinative set. Determinative sets are important for creating causal diagrams (in the form of directed
#'  acyclic graphs) from SCC models. VanderWeele and Robins (2007) write (p. 1099, D refers to the outcome):
#'
#'  *"To ensure that the DAG with the sufficient causation structure is itself a causal DAG, it is important that the set of sufficient causes for*
#'  *D on the graph be a determinative set of sufficient causes — that is, that the sufficient causes represent all of the pathways by which the*
#'  *outcome D may occur. Otherwise certain nodes may have common causes which are not on the graph, and the graph will then not be a causal DAG."*
#'
#'  It can of course be argued that an unknown sufficient cause in the described form is hardly of any use when creating a causal graph (as a DAG)
#'  from a SCC model. Nonetheless, it can be, as mentioned, a placeholder and reminder of limited knowledge.
#'  }
#'
#'  \item{`Sufficiency status`}{The sufficiency status describes under which circumstances a certain set of component causes is sufficient. There
#'  are 5 possible values:
#'  * `always`: The set of component causes is always sufficient.
#'  * `depends`: The set of component causes is sometimes sufficient and sufficiency depends on the order of occurrence of the involved steps,
#'    because some of them contain IFNOT conditions. However, if an IFNOT condition prevents the step from happening depends on the order of
#'    occurrence: if the IF condition is fulfilled before the IFNOT condition, the step (usually) occurs anyways, similar to how I do not care if
#'    a door is closed if I already went through it when it was still open.
#'  * `depends (potential order implausibilities)`: Same as "depends", but in the list of potential orders of occurrence of the involved steps,
#'    there might be some that do not make sense in practice, e.g., when two steps with IFNOT conditions are chained together: Imagine Step1
#'    having IF condition If1 and IFNOT condition Ifnot1, and Step2 having IF condition If2 and IFNOT condition Step1. The order Step1 -> Ifnot1
#'    -> If1 -> If2 is not plausible because Ifnot1 occurred before If1 and therefore Step1 did never occur. The user needs to discard these orders
#'    of occurrence (as I am currently not confident to correctly remove only implausible ones with code).
#'  * `never`: The set of component causes is never sufficient. This status is not used in `epicmodel_scc`. It's only used when investigating the
#'    effect of interventions (see [intervene()]).
#'  * `unknown`: This is the status of the unknown sufficient cause, which is added to the SCC model. It's only used in element `unknown_status` of
#'    `epicmodel_scc` objects.}
#'  }
#'
#' @param x `x` is used in several functions:
#' * `new_scc()`: A list to be converted to class `epicmodel_scc`.
#' * `validate_scc()`: An object of class `epicmodel_scc` to be validated.
#' * `print.epicmodel_scc()`: An object of class `epicmodel_scc`.
#' * `plot.epicmodel_scc()`: An object of class `epicmodel_scc`.
#' @param ... Additional arguments for generics `print()`, `summary()`, and `plot()`.
#'
#' @returns
#' * `new_scc()`: An object of class `epicmodel_scc`.
#' * `validate_scc()`: An object of class `epicmodel_scc` that has been checked to have the correct structure.
#' * `empty_scc()`: A (realtively) empty object of class `epicmodel_scc` with correct structure.
#' * `print.epicmodel_scc()`: Prints a summary of the object of class `epicmodel_scc` in the console.
#' * `summary.epicmodel_scc()`: Same as `print.epicmodel_scc()`.
#' * `plot.epicmodel_scc()`: A `ggplot` object.
#'
#' @seealso
#' * [create_scc()] for information on the algorithm for creating SCC models
#' * [plot_dag()] for how determinative sets of component causes are displayed in a DAG
#' * [intervene()] for the use of sufficiency status⁠ "never"
#'
#' @references
#' * Rothman KJ, Greenland S, Poole C, Lash TL (2008): Causation and Causal Inference. In: Rothman KJ, Greenland S, Lash TL (Ed.): Modern
#' epidemiology. Third edition. Philadelphia, Baltimore, New York: Wolters Kluwer Health Lippincott Williams & Wilkins, pp. 5–31.
#' * VanderWeele TJ, Robins JM (2007): Directed acyclic graphs, sufficient causes, and the properties of conditioning on
#' a common effect. American Journal of Epidemiology 166 (9): 1096–1104.
#'
#' @rdname epicmodel_scc
#'
#' @export
#'
#' @examples
#' # epicmodel_scc object are created by create_scc()
#'
#' # first, check your steplist of choice
#' steplist_checked <- check_steplist(steplist_rain)
#' # then, use it in create_scc()
#' scc_model <- create_scc(steplist_checked)
#'
#' # new_scc() and validate_scc() are used inside create_scc()
#' # nonetheless, you can check its structure with validate_scc()
#' validate_scc(scc_model)
#'
#' # print() and summary() both summarize the model in the console
#' print(scc_model)
#' scc_model
#' summary(scc_model)
#'
#' # plot causal pies with plot()
#' plot(scc_model)
#'
new_scc <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = "epicmodel_scc")
}

#' @description `new_scc()`, `validate_scc()`, and `empty_scc()` define the S3 class.
#'
#' @rdname epicmodel_scc
#'
#' @export
#'
validate_scc <- function(x) {
  # S3 class
  checkmate::assert_class(x, "epicmodel_scc", null.ok = F)

  # Set up checkmate collection
  coll <- checkmate::makeAssertCollection()

  # Element names
  checkmate::assert_set_equal(names(x), c("sc_cc","sc_status","sc_steps","sc_order","sc_implausibilities","sc_implausibilities_detail",
                                          "sc_use_modules","unknown_cc","unknown_status","steplist"), add = coll)

  # sc_cc
  checkmate::assert_data_frame(x$sc_cc, types = "logical", any.missing = F, null.ok = F, min.cols = 1, min.rows = 1, col.names = "unique",
                               row.names = "unique", add = coll)
  checkmate::assert_subset(colnames(x$sc_cc), x$steplist$step$id_step, add = coll)
  checkmate::assert_character(rownames(x$sc_cc), pattern = "^cc[[:digit:]]+$", add = coll)

  # sc_status
  checkmate::assert_character(x$sc_status, any.missing = F, null.ok = F, min.len = 1, min.chars = 1, names = "unique", add = coll)
  checkmate::assert_subset(x$sc_status, c("always", "depends", "depends (potential order implausibilities)"), add = coll)
  checkmate::assert_character(names(x$sc_status), pattern = "^cc[[:digit:]]+$", add = coll)

  # sc_steps
  checkmate::assert_list(x$sc_steps, types = "character", any.missing = F, null.ok = F, min.len = 1, names = "unique", add = coll)
  checkmate::assert_character(names(x$sc_steps), pattern = "^cc[[:digit:]]+$", add = coll)
  for (i in 1:length(x$sc_steps)) {
    checkmate::assert_character(x$sc_steps[[i]], any.missing = F, null.ok = F, min.len = 1, min.chars = 1, add = coll)
    checkmate::assert_subset(x$sc_steps[[i]], x$steplist$step$id_step, add = coll)
  }

  # sc_order
  checkmate::assert_list(x$sc_order, types = c("data.frame", "logical"), any.missing = T, null.ok = F, min.len = 1, names = "unique", add = coll)
  checkmate::assert_character(names(x$sc_order), pattern = "^cc[[:digit:]]+$", add = coll)
  for (i in 1:length(x$sc_order)) {
    if (x$sc_status[[names(x$sc_order)[i]]] %>% magrittr::is_in(c("depends", "depends (potential order implausibilities)"))) {
      checkmate::assert_data_frame(x$sc_order[[i]], types = c("character", "logical"), any.missing = F, null.ok = F, ncols = 2, min.rows = 1,
                                   col.names = "unique", add = coll)
      checkmate::assert_set_equal(colnames(x$sc_order[[i]]), c("order", "suff"), ordered = T, add = coll)
    } else {
      checkmate::assert_scalar_na(x$sc_order[[i]], null.ok = F, add = coll)
    }
  }

  # sc_implausibilities
  checkmate::assert_logical(x$sc_implausibilities, any.missing = F, null.ok = F, min.len = 1, names = "unique", add = coll)
  checkmate::assert_character(names(x$sc_implausibilities), pattern = "^cc[[:digit:]]+$", add = coll)

  # sc_implausibilities_detail
  checkmate::assert_list(x$sc_implausibilities_detail, types = c("character", "logical"), any.missing = T, null.ok = F, min.len = 1,
                         names = "unique", add = coll)
  checkmate::assert_character(names(x$sc_implausibilities_detail), pattern = "^cc[[:digit:]]+$", add = coll)
  for (i in 1:length(x$sc_implausibilities_detail)) {
    if (x$sc_implausibilities[[names(x$sc_implausibilities_detail)[i]]]) {
      checkmate::assert_character(x$sc_implausibilities_detail[[i]], any.missing = F, null.ok = F, min.len = 1, min.chars = 1, add = coll)
      checkmate::assert_subset(x$sc_implausibilities_detail[[i]], x$steplist$then$id_then, add = coll)
    } else {
      checkmate::assert_scalar_na(x$sc_implausibilities_detail[[i]], null.ok = F, add = coll)
    }
  }

  # sc_use_modules
  checkmate::assert_logical(x$sc_use_modules, any.missing = F, null.ok = F, len = 1, add = coll)

  # unknown_cc
  checkmate::assert_data_frame(x$unknown_cc, types = "logical", any.missing = F, null.ok = F, min.cols = 3, min.rows = 2, col.names = "unique",
                               row.names = "unique", add = coll)
  checkmate::assert_subset(colnames(x$unknown_cc)[1:(ncol(x$unknown_cc) - nrow(x$unknown_cc))], x$steplist$step$id_step, add = coll)
  checkmate::assert_set_equal(colnames(x$unknown_cc)[(ncol(x$unknown_cc) - nrow(x$unknown_cc) + 1):ncol(x$unknown_cc)],
                              c(paste0("U",c(1:nrow(x$sc_cc))),"USC"), add = coll)
  checkmate::assert_character(rownames(x$unknown_cc), pattern = "^cc[[:digit:]]+$", add = coll)
  checkmate::assert_set_equal(rownames(x$unknown_cc)[nrow(x$unknown_cc)], "cc0", add = coll)

  # unknown_status
  checkmate::assert_character(x$unknown_status, any.missing = F, null.ok = F, len = length(x$sc_status) + 1, min.chars = 1,
                              names = "unique", add = coll)
  checkmate::assert_set_equal(x$unknown_status, c(x$sc_status, "unknown"), add = coll)
  checkmate::assert_set_equal(names(x$unknown_status), c(names(x$sc_status), "cc0"), add = coll)

  # steplist
  checkmate::assert_class(x$steplist, "epicmodel_steplist_checked", null.ok = F)

  # Report collection
  checkmate::reportAssertions(coll)

  return(x)
}

#' @rdname epicmodel_scc
#'
#' @export
#'
empty_scc <- function() {
  x <- vector(mode = "list", length = 10)

  sc_cc <- matrix(c(TRUE,TRUE), nrow = 1, ncol = 2) %>% as.data.frame() %>% magrittr::set_colnames(c("THENa1","THENa2")) %>%
    magrittr::set_rownames("cc3")

  sc_status <- c(cc3 = "always")

  sc_steps <- list(cc3 = c("THENa1", "THENa2", "IFa1+a2THENa3"))

  sc_order <- list(cc3 = NA)

  sc_implausibilities <- c(cc3 = FALSE)

  sc_implausibilities_detail <- list(cc3 = NA)

  sc_use_modules <- FALSE

  unknown_cc <- matrix(c(T,T,T,F,F,F,F,T), nrow = 2, ncol = 4, byrow = T) %>% as.data.frame() %>%
    magrittr::set_colnames(c("THENa1","THENa2","U1","USC")) %>% magrittr::set_rownames(c("cc3","cc0"))

  unknown_status <- c(cc3 = "always", cc0 = "unknown")

  steplist <- empty_steplist()
  steplist$what %<>% dplyr::filter(.data$id_what == "x")
  steplist$does %<>% dplyr::filter(.data$id_does == "x")
  steplist$where %<>% dplyr::filter(.data$id_where == "x")
  steplist$then %<>% dplyr::filter(.data$id_then == "x")
  steplist$module %<>% dplyr::filter(.data$id_module == "x")
  steplist$step %<>% dplyr::filter(.data$id_step == "x")
  steplist$icc %<>% dplyr::filter(.data$id_icc == "x")
  steplist$outc %<>% dplyr::filter(.data$id_outc == "x")

  steplist$what %<>% rbind(.,data.frame(id_what = c("a1","a2","a3"), key_what = c("a1","a2","a3"),
                                        desc_what = c("a1","a2","a3"), plural_what = c("0","0","0")))
  steplist$then %<>% rbind(.,data.frame(id_then = c("a1","a2","a3"), desc_then = c("a1","a2","a3")))
  steplist$step %<>% rbind(.,data.frame(id_step = c("THENa1","THENa2","IFa1+a2THENa3"),
                                        desc_step = c("Start: a1","Start: a2","End: IF a1 and a2 THEN a3"),
                                        end_step = c("0","0","1"), module_step = c("","",""),
                                        note_step = c("","",""), ref_step = c("","","")))
  steplist$outc %<>% rbind(.,data.frame(id_outc = c("a3"), desc_outc = c("a3")))

  steplist %<>% unclass(.)
  steplist %<>% structure(., class = "epicmodel_steplist_checked")

  x <- list(sc_cc = sc_cc, sc_status = sc_status, sc_steps = sc_steps, sc_order = sc_order, sc_implausibilities = sc_implausibilities,
            sc_implausibilities_detail = sc_implausibilities_detail, sc_use_modules = sc_use_modules, unknown_cc = unknown_cc,
            unknown_status = unknown_status, steplist = steplist)

  x %<>% new_scc()
  x %<>% validate_scc()

  return(x)
}

#' @description `print()` prints a summary of SCC models in the console. `summary()` and `print()` are identical.
#'
#' @method print epicmodel_scc
#'
#' @rdname epicmodel_scc
#'
#' @export
#'
print.epicmodel_scc <- function(x, ...) {
  # Get names and component causes of sufficient causes
  sc_names <- x$sc_status %>% names()
  cc <- x %>% scc_cause_sets(output = "desc_no_start", depends = T)

  # Get module prevalences
  if (x$sc_use_modules) {
    module_prev <- x %>% module_prev()
  }

  # Start printing by listing outcome definitions
  cli::cli_h2("Outcome Definitions")
  cli::cli_ul(x$steplist$outc$desc_outc)

  # Loop over every sufficient cause
  for (i in 1:length(sc_names)) {
    ## Header for sufficient cause
    cli::cli_h2(paste("SC",i))
    ## Status
    if (x$sc_status[sc_names[i]] == "always") {cli::cli_alert_success("Always sufficient")}
    if (x$sc_status[sc_names[i]] == "depends (potential order implausibilities)" | x$sc_status[sc_names[i]] == "depends") {
      cli::cli_alert_warning("Sufficiency depends on order of occurrence")
    }
    ## List component causes
    cli::cli_par()
    cli::cli_text("Component causes:")
    cli::cli_ul(cc[[sc_names[i]]])
    cli::cli_end()
    ## If suffciency depends on order, print a list of orders that are sufficient
    if (!is.na(x$sc_order[sc_names[i]]) &
        (x$sc_status[sc_names[i]] == "depends (potential order implausibilities)" | x$sc_status[sc_names[i]] == "depends")) {
      cli::cli_par()
      cli::cli_text("Sufficient orders of occurrence:")
      ### Warn of implausible orders if applicable
      if (x$sc_implausibilities[sc_names[i]]) {cli::cli_alert_warning("{.field Please note:} There might be implausible orders of occurrence!")}
      ### Use sufficient_order() to prepare list
      cli::cli_ul(sufficient_order(x$sc_order[[sc_names[i]]], x$steplist, sufficient = TRUE))
      cli::cli_end()
    }
    ## List module prevalence if applicable
    if (x$sc_use_modules) {
      cli::cli_par()
      cli::cli_text("Modules")
      cli::cli_ul(module_prev[[sc_names[i]]])
      cli::cli_end()
    }
  }
}

#' @param object For `summary.epicmodel_scc()`, an object of class `epicmodel_scc`.
#'
#' @method summary epicmodel_scc
#'
#' @rdname epicmodel_scc
#'
#' @export
#'
summary.epicmodel_scc <- function(object, ...) {
  print(object)
}

#' @description `plot()` creates the familiar causal pie charts from an object of class `epicmodel_scc`.
#'
#' @param remove_sc For `plot.epicmodel_scc()`, a vector of integerish numbers, i.e., integers that can be specified as numeric, i.e., `1` and `1L`
#' are both possible. Removes the sufficient cause (SC) with the specified index from the plot, i.e., for `remove_sc = 2`, removes SC 2, and for
#' `remove_sc = c(2,3)`, removes SC 2 and SC 3. If there are x sufficient causes in the model, x is the highest allowed value. At least one
#' sufficient cause needs to remain, i.e., not all sufficient causes can be removed. If NULL (default), all sufficient causes are plotted.
#' @param sc_label For `plot.epicmodel_scc()`, a character vector with the labels written above the pies, i.e., sufficient causes. If NULL
#' (default), "Sufficient Cause 1", "Sufficient Cause 2", etc. are used. If specified, try to provide as many labels as there are pies in the plot.
#' Duplicates are not allowed.
#' @param unknown For `plot.epicmodel_scc()`, TRUE (default) or FALSE. If TRUE, unknown causes are added to the SCC model: every sufficient cause
#' gets an additional individual unknown component cause representing additional unknown components; an unknown sufficient cause is added to
#' the model consisting of a single unknown component cause and representing all unknown sufficient causes.
#' @param names For `plot.epicmodel_scc()`, TRUE (default) or FALSE. If TRUE, includes the translation of pie segment names to descriptions of
#' component causes in the plot.
#' @param text_color For `plot.epicmodel_scc()`, a single element of type character, which is a valid color description. Valid color descriptions
#' can be named colors ("white") or hexadecimal color codes ("#FFFFFF"). `text_color` will be used for the pie segment names. If NULL (default),
#' "white" is used.
#' @param pie_color For `plot.epicmodel_scc()`, a character vector of length 3 containing valid color descriptions. Valid color descriptions
#' can be named colors ("white") or hexadecimal color codes ("#FFFFFF"). The first element of `pie_color` is used to color sufficient causes,
#' which are always sufficient. The second element is used to color sufficient causes, for which sufficiency depends on the order of occurrence.
#' The third element is used to color the unknown sufficient cause, which is present if `unknown` is TRUE. If NULL (default), the following colors
#' are used: "#B1934A", "#A65141", "#394165"
#' @param border_color For `plot.epicmodel_scc()`, a single element of type character, which is a valid color description. Valid color
#' descriptions can be named colors ("white") or hexadecimal color codes ("#FFFFFF"). `border_color` will be used for all pie borders apart from
#' the unknown sufficient cause. Therefore, only specify `border_color` if `unknown` is FALSE. If NULL (default), "white" is used. (Borders
#' for the unknown sufficient cause have the same color as the pie.)
#'
#' @rdname epicmodel_scc
#'
#' @export
#'
plot.epicmodel_scc <- function(x, remove_sc = NULL, sc_label = NULL, unknown = TRUE, names = TRUE, text_color = NULL, pie_color = NULL,
                               border_color = NULL, ...) {
  # Check input
  rlang::try_fetch(checkmate::assert_integerish(remove_sc, null.ok = T, any.missing = F, min.len = 1, max.len = nrow(x$sc_cc) - 1,
                                                lower = 1, upper = nrow(x$sc_cc)),
                   error = function(cnd) {
                     cli::cli_abort(c("{.var remove_sc} must be a vector of integers!",
                                      "i" = "Allowed are numbers from 1 up to the number of sufficient causes in the plotted model.",
                                      "i" = "You cannot remove all sufficient causes from the plot."),
                                    parent = cnd, class = "input_remove_sc")
                   })

  rlang::try_fetch(checkmate::assert_character(sc_label, null.ok = T, any.missing = F, min.len = 1, min.chars = 1, unique = T),
                   error = function(cnd) {
                     cli::cli_abort(c("{.var sc_label} must be a character vector!",
                                      "i" = ""),
                                    parent = cnd, class = "input_sc_label")
                   })

  rlang::try_fetch(checkmate::assert_logical(unknown, any.missing = F, len = 1, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var unknown} must be TRUE or FALSE!", parent = cnd, class = "input_unknown")
                   })

  rlang::try_fetch(checkmate::assert_logical(names, any.missing = F, len = 1, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var names} must be TRUE or FALSE!", parent = cnd, class = "input_names")
                   })

  rlang::try_fetch(checkmate::assert_character(text_color, any.missing = F, len = 1, min.chars = 1, null.ok = T),
                   error = function(cnd) {
                     cli::cli_abort(c("{.var text_color} must be a single element of type character holding a valid color description!",
                                      "i" = "Valid color descriptions can be named colors ('white') or hexadecimal color codes ('#FFFFFF')."),
                                    parent = cnd, class = "input_text_color")
                   })

  if (!is.null(text_color)) {
    if (!are_colors(text_color)) {
      cli::cli_abort("{.var text_color} contains invalid color descriptions!", class = "invalid_text_color")
    }
  }

  rlang::try_fetch(checkmate::assert_character(pie_color, any.missing = F, len = 3, min.chars = 1, null.ok = T),
                   error = function(cnd) {
                     cli::cli_abort(c("{.var pie_color} must be character vector of length 3 holding valid color descriptions!",
                                      "i" = "Valid color descriptions can be named colors ('white') or hexadecimal color codes ('#FFFFFF')."),
                                    parent = cnd, class = "input_pie_color")
                   })

  if (!is.null(pie_color)) {
    if (!are_colors(pie_color)) {
      cli::cli_abort("{.var pie_color} contains invalid color descriptions!", class = "invalid_pie_color")
    }
  }

  rlang::try_fetch(checkmate::assert_character(border_color, any.missing = F, len = 1, min.chars = 1, null.ok = T),
                   error = function(cnd) {
                     cli::cli_abort(c("{.var border_color} must be an element of type character holding a valid color description!",
                                      "i" = "Valid color descriptions can be named colors ('white') or hexadecimal color codes ('#FFFFFF')."),
                                    parent = cnd, class = "input_border_color")
                   })

  if (!is.null(border_color)) {
    if (!are_colors(border_color)) {
      cli::cli_abort("{.var border_color} contains invalid color descriptions!", class = "invalid_border_color")
    }

    if (unknown) {
      cli::cli_warn("It is recommended to specify {.var border_color} only if {.var unknown} equals FALSE!", class = "unknown_plus_border_color")
    }
  }
  #=============================================================================
  # Select input with or without unknown causes
  if (unknown) {
    cc <- x$unknown_cc
    status <- x$unknown_status
  } else {
    cc <- x$sc_cc
    status <- x$sc_status
  }
  status[status == "depends (potential order implausibilities)"] <- "depends"

  # Prepare plot data
  ## Empty container
  df_plot <- vector(mode = "list", length = nrow(cc))
  ## Fill container for every sufficient cause
  for (i in 1:length(df_plot)) {
    temp <- cc[i,] %>% t() %>% magrittr::set_colnames("included") %>% as.data.frame() %>% dplyr::filter(.data$included == TRUE) %>%
      tibble::rownames_to_column("id")
    temp$frac <- 1/nrow(temp)
    temp$cause <- paste("Sufficient Cause",i)
    temp$Sufficiency <- status[rownames(cc)[i]]
    if (status[rownames(cc)[i]] == "unknown") {
      temp$known <- "unknown"
    } else {
      temp$known <- "known"
    }
    temp$included <- NULL
    df_plot[[i]] <- temp
  }
  ## Make big data.frame from list of data.frames
  df_plot %<>% purrr::map_dfr(as.data.frame)
  df_plot$Sufficiency %<>% factor(levels = c("always","depends","unknown"))
  df_plot$known %<>% factor(levels = c("known", "unknown"))
  ## Remove SCs
  if (!is.null(remove_sc)) {
    df_plot %<>% dplyr::filter(!(.data$cause %in% paste("Sufficient Cause",remove_sc)))
  }

  # Translate SC label
  if (!is.null(sc_label)) {
    sc_labels_ols <- df_plot$cause %>% unique() %>% sort()
    for (i in 1:length(sc_label)) {
      df_plot$cause[df_plot$cause == sc_labels_ols[i]] <- sc_label[i]
    }
    if (length(sc_labels_ols) == length(sc_label)) {
      df_plot$cause %<>% factor(levels = sc_label, labels = sc_label)
    }
  }

  # Translate labels
  LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
  if (unknown) {
    label_cols <- colnames(cc)[colnames(cc) %>% stringr::str_detect("^U", negate = T) & colnames(cc) %>% magrittr::is_in(df_plot$id)]
    label_unknowns <- colnames(cc)[colnames(cc) %>% stringr::str_detect("^U", negate = F)]
    label_translator <- data.frame(id = label_cols, label = LETTERS702[1:length(label_cols)])
    label_translator %<>% rbind(., data.frame(id = label_unknowns, label = label_unknowns))
  } else {
    label_cols <- colnames(cc)[colnames(cc) %>% stringr::str_detect("^U", negate = T) & colnames(cc) %>% magrittr::is_in(df_plot$id)]
    label_translator <- data.frame(id = label_cols, label = LETTERS702[1:length(label_cols)])
  }
  ## Add labels to plot data
  df_plot %<>% dplyr::left_join(label_translator, by = "id")
  df_plot$label %<>% factor()

  # Prepare caption
  label_translator$desc <- NA_character_
  label_translator %<>% dplyr::filter(.data$id %>% stringr::str_detect("^U", negate = T))
  for (i in 1:nrow(label_translator)) {
    label_translator$desc[i] <- x$steplist$step$desc_step[x$steplist$step$id_step == label_translator$id[i]] %>% stringr::str_replace("^Start: ","")
  }
  label_translator$id <- NULL
  label_translator$caption <- paste0(label_translator$label,": ",label_translator$desc)
  caption <- label_translator[["caption"]] %>% stringr::str_c(collapse = "\n")

  # Define colors
  if (is.null(text_color)) {
    text_color <- "white"
  }
  if (is.null(pie_color)) {
    pie_color <- c("#B1934A", "#A65141", "#394165")
  }
  if (is.null(border_color)) {
    border_color <- "white"
  }

  # Plot
  plot <- df_plot %>%
    ggplot2::ggplot(ggplot2::aes(x = 0, y = .data$frac, fill = .data$Sufficiency, color = .data$known)) +
    ggplot2::geom_bar(stat = "identity", linewidth = 1) +
    ggplot2::scale_fill_manual(values = c("always" = pie_color[1],"depends" = pie_color[2], "unknown" = pie_color[3]), guide = "none") +
    ggplot2::scale_color_manual(values = c("known" = border_color, "unknown" = pie_color[3]), guide = "none") +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), col = text_color, position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom", plot.caption = ggplot2::element_text(hjust = 0)) +
    ggplot2::facet_wrap(~cause)
  ## Only add caption if caption == TRUE
  if (names) {
    plot <- plot + ggplot2::labs(caption = caption)
  }
  ## Only add legend when status "depends" or "unknown" appears (i.e., not only "always")
  if (df_plot$Sufficiency %>% magrittr::equals("always") %>% all_true() %>% magrittr::not()) {
    plot <- plot + ggplot2::guides(fill = "legend")
  }

  plot
}

#' Sufficient orders of SCC model
#'
#' If a SCC model has sufficient causes for which sufficiency depends on the order of occurrence, displays the sufficient ones in a nicely manner
#' for use in `print.epicmodel_scc()` and `nice_output_intervene()`.
#'
#' @param order An element from the `sc_order` list of an `epicmodel_scc` object, which is a data.frame with columns `order` and `suff`
#' representing a single sufficient cause. If the sufficient cause is always sufficient, the element is not a data.frame but simply NA.
#' @param steplist An object of class `epicmodel_steplist_checked`, which is element `steplist` from the `epicmodel_scc` object from where
#' `order` originates as well.
#' @param sufficient TRUE (default) or FALSE. If TRUE, extracts the orders of occurrence that are sufficient (for printing SCC models). If FALSE,
#' extracts the orders of occurrence that are not sufficient (for printing interventions, since the intervention was successful, when the outcome
#' doe not occur).
#'
#' @returns A vector of type character with each element representing one sufficient ordering for this sufficient cause for use with cli_ul().
#'
#' @noRd
sufficient_order <- function(order, steplist, sufficient = TRUE) {
  # Check input
  if (order %>% is.na() %>% all_true()) {
    rlang::try_fetch({
      checkmate::assert_scalar_na(order, null.ok = F)
      }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var order}",
                                                 "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                               parent = cnd, class = "input_order")
    })
  } else {
    rlang::try_fetch({
      checkmate::assert_data_frame(order, types = c("character", "logical"), any.missing = F, null.ok = F, ncols = 2, min.rows = 1,
                                   col.names = "unique")
      checkmate::assert_set_equal(colnames(order), c("order", "suff"), ordered = T)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var order}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_order")
    })
  }

  rlang::try_fetch({
      checkmate::assert_class(steplist, "epicmodel_steplist_checked", null.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var steplist}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_steplist")
  })

  rlang::try_fetch({
      checkmate::assert_logical(sufficient, any.missing = F, len = 1, null.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var sufficient}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_sufficient")
  })
  #=============================================================================
  # Differentiate between NA and actual data.frames with order information
  if (is.data.frame(order)) {
    ## Filter for sufficient orders of occurrence
    order_suff <- order %>% dplyr::filter(.data$suff == sufficient)
    ## Return NA if there are no sufficient orders of occurrence
    if (nrow(order_suff) == 0) {
      return(NA_character_)
    }
    ## Split the orders of occurrence into their parts, i.e., the IDs of IF conditions, IFNOT conditions, or THEN statements
    order_list <- order_suff$order %>% stringr::str_split("->")
    ## Get to actual THEN statement IDs that are part of the order elements and change IDs to descriptions
    order_list_sep <- order_list %>% purrr::map(sep_if_ifnot)
    for (i in 1:length(order_list_sep)) {
      for (j in 1:length(order_list_sep[[i]])) {
        for (k in 1:nrow(order_list_sep[[i]][[j]])) {
          order_list_sep[[i]][[j]][k,"id"] <- steplist$then$desc_then[steplist$then$id_then == order_list_sep[[i]][[j]][k,"id"]]
        }
      }
    }
    ## Create output container and put the conditions back together with descriptions instead of IDs
    out <- rep(NA_character_, length = length(order_list_sep))
    for (i in 1:length(order_list_sep)) {
      out2 <- rep(NA_character_, length(order_list_sep[[i]]))
      for (j in 1:length(order_list_sep[[i]])) {
        temp <- order_list_sep[[i]][[j]]
        max_sce_temp <- temp$sce %>% as.numeric() %>% max()
        out3 <- rep(NA_character_, max_sce_temp)
        for (k in 1:max_sce_temp) {
          temp2 <- temp %>% dplyr::filter(.data$sce == k)
          out3[k] <- temp2$id %>% stringr::str_c(collapse = " and ")
          if (max_sce_temp > 1) {
            out3[k] %<>% paste0("(",.,")")
          }
        }
        out2[j] <- out3 %>% stringr::str_c(collapse = " or ")
      }
      out[i] <- out2 %>% stringr::str_c(collapse = " -> ")
    }
    #=============================================================================
    # Check output
    rlang::try_fetch({
        checkmate::assert_character(out, null.ok = F, any.missing = F, min.len = 1, min.chars = 1)
      }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                                 "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                               parent = cnd, class = "output")
    })
    #=============================================================================
    return(out)
  } else {
    return(NA_character_)
  }
}

#' Extract module prevalence
#'
#' Extracts the module prevalence for every sufficient cause, i.e., how many
#' steps are part of each module. Displays the result in a nicely manner for use
#' in `print.epicmodel_scc()`.
#'
#' @param scc An object of class `epicmodel_scc`.
#'
#' @returns A named list with length equal to the number of sufficient causes in `scc` and names equal to the sufficient cause names, e.g. cc1, etc.
#' List elements are character vectors with one element for every module that appears in the corresponding sufficient cause. The vector contains
#' nicely formatted information on how many steps of the corresponding sufficient cause belong to each module.
#'
#' @noRd
module_prev <- function(scc) {
  # Check inputs
  if (inherits(scc, "epicmodel_scc") %>% magrittr::not()) {
    cli::cli_abort("{.var scc} must be a {.emph epicmodel_scc} class object!",
                   class = "no_scc")
  }

  if (!scc$sc_use_modules) {
    cli::cli_abort("The SCC does not use module!", class = "no_modules")
  }
  #=============================================================================
  # Process steplist
  prc <- scc$steplist %>% process_steplist()

  # Create and fill output container
  out <- vector(mode = "list", length = length(scc$sc_steps)) %>% magrittr::set_names(names(scc$sc_steps))
  for (i in 1:length(scc$sc_steps)) {
    prc_temp <- prc %>% dplyr::filter(.data$id_step %in% scc$sc_steps[[names(out)[i]]]) %>% dplyr::select(dplyr::all_of(c("id_step","module_step")))
    prc_temp %<>% dplyr::left_join(scc$steplist$module, by = c("module_step" = "id_module"))
    prc_temp$key_module %<>% factor()

    out[[i]] <- summary(prc_temp$key_module) %>% sort(., decreasing = T) %>%
      paste0(names(.),": ",round(./sum(.)*100, digits = 0),"% (",.,"/",sum(.),")")
  }
  #=============================================================================
  # Check output
  rlang::try_fetch({
    checkmate::assert_list(out, types = "character", any.missing = F, null.ok = F, len = length(scc$sc_status), names = "unique")
    checkmate::assert_set_equal(names(out), names(scc$sc_status))
    }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output")
  })

  for (i in 1:length(out)) {
    rlang::try_fetch({
      checkmate::assert_character(out[[i]], any.missing = F, null.ok = F, min.len = 1, min.chars = 1)
      }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                                 "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                               parent = cnd, class = "output")
    })
  }
  #=============================================================================
  return(out)
}
