#' Steplist objects
#'
#' The S3 classes `epicmodel_steplist` and `epicmodel_steplist_checked` store the input information for SCC model creation. They are created from
#' the Steplist Creator `shiny` app, which can be launched with [launch_steplist_creator()].
#'
#' @details
#' ## `epicmodel_steplist` objects
#' `epicmodel_steplist` objects are lists containing 8 data.frames. These data.frames are described below:
#'  \describe{
#'   \item{`what`}{A list of subjects and objects (WHAT segments) appearing in the step descriptions, e.g., cells, interleukins, symptoms, etc.,
#'   with the following variables:
#'  * id_what: Automatically created ID for WHAT segments. Starts with "a" followed by a number, e.g., a1. Used in creating automatic step IDs.
#'  * key_what: Keyword describing the WHAT segment. Used in steplist_creator shiny app dropdown menus.
#'  * desc_what: Text used in step descripiton.
#'  * plural_what: Indicates if plural (1) or singular (0) version of the DOES description should be used, if this WHAT segment is used as subject,
#'    i.e., WHAT segment before the DOES segment.}
#'
#'   \item{`does`}{A list of actions or verbs (DOES segments), with which the WHAT segments interact, e.g., is present, produce, migrate,
#'   exposed to, with the following variables:
#'   * id_does: Automatically created ID for DOES segments. Starts with "d" followed by a number, e.g., d1. Used in creating automatic step IDs.
#'   * key_does: Keyword describing the DOES segment. Used in steplist_creator shiny app dropdown menus.
#'   * subject_singular_does: Description used if subject (WHAT segment in front) has been specified as singular (plural_what=0).
#'   * subject_plural_does: Description used if subject (WHAT segment in front) has been specified as plural (plural_what=1).
#'   * no_subject_does: Description used if no subject (WHAT segment in front) has been specified.
#'   * then_object_does: Indicates if the object for this DOES segment is a WHAT segment (0) or a THEN statement (1).}
#'
#'   \item{`where`}{A list of locations (WHERE segments), where the specified actions take place, e.g., in the airways, with the following
#'   variables:
#'   * id_where: Automatically created ID for WHERE segments. Starts with "e" followed by a number, e.g., e1. Used in creating automatic step IDs.
#'   * key_where: Keyword describing the WHERE segment. Used in steplist_creator shiny app dropdown menus.
#'   * desc_where: Text used in step descripiton. Please include the corresponding preposition, e.g., 'in', 'into', 'on', etc.}
#'
#'   \item{`then`}{A list of combinations of WHAT, DOES and WHERE segments (THEN statements). A THEN statement can contain up to 4 segments: WHAT
#'   (subject), DOES, WHAT (object), WHERE. Not all 4 of them need to be specified. For some DOES segments, the corresponding object is not a WHAT
#'   segment but a THEN statement (see `then_object_does`). In general, all combinations are possible, although only DOES, only WHERE, and WHAT WHAT
#'   do not make a lot of sense. `then` exists to store the THEN statements that are later used in IF and IFNOT conditions. It contains the following
#'   variables:
#'   * id_then: Automatically created ID based on segment IDs, e.g., a4, a1d5a15e9, d2a3.
#'   * desc_then: Automatically created description based on segment descriptions.}
#'
#'   \item{`module`}{Modules are groups, into which the steps are sorted, e.g., immune system, lung, etc., as it is sometimes of interest to see
#'   which groups are involved in the sufficient causes. It contains the following variables:
#'   * id_module: Automatically created ID for modules. Starts with "m" followed by a number, e.g., m1.
#'   * key_module: Keyword describing the module.
#'   * desc_module: Module description.}
#'
#'   \item{`step`}{Main table of interest and the one further processed to create sufficient-component cause models. It contains the following
#'   variables:
#'   * id_step: Automatically created step ID based on IDs of included THEN statements, e.g., IFd6a10IFNOTd6a18+d1a8THENa11d3a12.
#'   * desc_step: Automatically created step description based on descriptions of included THEN statements.
#'   * end_step: Indicator variable that describes if this step is at the end of a certain sub-mechanism, e.g., symptom x occured.
#'   * module_step: Module, i.e., group, into which this step has been sorted.
#'   * note_step: Additional notes that are important for future users, e.g., if there are conflicting results or if the result is from a mouse
#'     model.
#'   * ref_step: References on which this step is based.}
#'
#'   \item{`icc`}{ICC is short for incompatibel component causes. It contains pairs of component causes, i.e., steps without IF or IFNOT condition,
#'   that are not compatible with each other, i.e., cannot appear in the same sufficient cause. It contains the following variables:
#'   * id_icc: Automatically created ID for ICC pairs. Starts with "i" followed by a number, e.g., i1.
#'   * id1: Step ID of first component cause.
#'   * id2: Step ID of second component cause.
#'   * desc1: Step description of first component cause.
#'   * desc2: Step description of second component cause.}
#'
#'   \item{`outc`}{A list that contains conditions under which the outcome of interest is assumed to occur. Each line might contain one or more THEN
#'   statements, that have been marked as end steps by setting step$end_step to 1. If more than one THEN statement is selected, they are combined
#'   with AND logic. All lines in this table are combined with OR logic, i.e., any of the specified conditions is assumed to represent outcome
#'   occurrence. The table contains the following variables:
#'   * id_outc: Automatically created ID for outcome definitions as a combination of the THEN statement IDs connected by '+'.
#'   * desc_outc: Automatically created description for the outcome definitions as a combination of the THEN statement descriptions.}
#'  }
#'
#'  ## `epicmodel_steplist_checked` objects
#'  Before using `epicmodel_steplist` object for SCC model creation in [create_scc()], they need to be checked for any structures that might make
#'  SCC model creation impossible. Checking is performed by [check_steplist()] and if successful, the returned object is of type
#'  `epicmodel_steplist_checked`. When changing the steplist in the Steplist Creator `shiny` app or by functions [remove_all_modules()],
#'  [remove_na()], or [remove_segment()], the steplist is "un-checked" and returned as class `epicmodel_steplist`. Apart from that, both classes
#'  have similar structure, which can be validated by `validate_steplist()`.
#'
#' @param x `x` is used in several functions:
#' * `new_steplist()`: A list to be converted to class `epicmodel_steplist`.
#' * `validate_steplist()`: An object of class `epicmodel_steplist` or `epicmodel_steplist_checked` to be validated.
#' * `print.epicmodel_steplist()`: An object of class `epicmodel_steplist`.
#' * `print.epicmodel_steplist_checked()`: An object of class `epicmodel_steplist_checked`.
#' * `plot.epicmodel_steplist()`: An object of class `epicmodel_steplist`.
#' * `plot.epicmodel_steplist_checked()`: An object of class `epicmodel_steplist_checked`.
#' @param ... Additional arguments for generics `print()`, `summary()`, and `plot()`.
#'
#' @returns
#' * `new_steplist()`: An object of class `epicmodel_steplist`.
#' * `validate_steplist()`: An object of class `epicmodel_steplist` or `epicmodel_steplist_checked`, that has been checked to have the correct
#'   structure.
#' * `empty_steplist()`: An empty object of class `epicmodel_steplist` object with correct structure.
#' * `print.epicmodel_steplist()`: Prints the number of entries in each data.frame in the console and the information that the steplist is
#'   `unchecked`.
#' * `print.epicmodel_steplist_checked()`: Same as `print.epicmodel_steplist()` but with the information the the steplist has been checked
#'   successfully.
#' * `summary.epicmodel_steplist()`: Prints an allert that the steplist needs to be checked with `check_steplist()` before using `summary()`.
#' * `summary.epicmodel_steplist_checked()`: Prints a list of steps by type of step in the console.
#' * `plot.epicmodel_steplist()`: Prints an allert that the steplist needs to be checked with `check_steplist()` before using `plot()`.
#' * `plot.epicmodel_steplist_checked()`: Prints a graph of the complete network of mechanisms in the RStudio Viewer and the corresponding legend
#'   in the console.
#'
#' @rdname epicmodel_steplist
#'
#' @export
#'
#' @examples
#' # Create steplists in the Steplist Creator `shiny` app
#' \dontrun{
#' launch_steplist_creator()
#'
#' # Download the steplist from the `shiny` app
#' # Load the steplist into R
#' steplist <- readRDS("folder/file.rds")
#' }
#'
#' # To familiarize yourself with the package you can use a built-in steplist
#' steplist <- steplist_rain
#'
#' # new_steplist(), validate_steplist(), and empty_steplist() are used in the `shiny` app
#' # nonetheless, you can check steplist structures with validate_steplist()
#' validate_steplist(steplist)
#'
#' # print() provides a summary of steplist entries and if it'S checked or unchecked
#' print(steplist)
#'
#' # Check steplist before using `summary()` and `plot()`
#' steplist_checked <- check_steplist(steplist)
#' summary(steplist_checked)
#' plot(steplist_checked)
new_steplist <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = "epicmodel_steplist")
}


#' @description `new_steplist()`, `validate_steplist()`, and `empty_steplist()` define the S3 class.
#'
#' @rdname epicmodel_steplist
#'
#' @export
#'
validate_steplist <- function(x) {
  if (inherits(x, c("epicmodel_steplist_checked","epicmodel_steplist")) %>% magrittr::not()) {
    cli::cli_abort("{.var x} must be a steplist object!", class = "no_steplist")
  }

  coll <- checkmate::makeAssertCollection()

  checkmate::assert_subset(names(x), c("what","does","where","then","module","step","icc","outc"), add = coll)

  checkmate::assert_data_frame(x[["what"]], ncols = 4, col.names = "named", add = coll)
  checkmate::assert_subset(colnames(x[["what"]]), c("id_what","key_what","desc_what","plural_what"), add = coll)

  checkmate::assert_data_frame(x[["does"]], ncols = 6, col.names = "named", add = coll)
  checkmate::assert_subset(colnames(x[["does"]]), c("id_does","key_does","subject_singular_does","subject_plural_does","no_subject_does",
                                                    "then_object_does"), add = coll)

  checkmate::assert_data_frame(x[["where"]], ncols = 3, col.names = "named", add = coll)
  checkmate::assert_subset(colnames(x[["where"]]), c("id_where","key_where","desc_where"), add = coll)

  checkmate::assert_data_frame(x[["then"]], ncols = 2, col.names = "named", add = coll)
  checkmate::assert_subset(colnames(x[["then"]]), c("id_then","desc_then"), add = coll)

  checkmate::assert_data_frame(x[["module"]], ncols = 3, col.names = "named", add = coll)
  checkmate::assert_subset(colnames(x[["module"]]), c("id_module","key_module","desc_module"), add = coll)

  checkmate::assert_data_frame(x[["step"]], ncols = 6, col.names = "named", add = coll)
  checkmate::assert_subset(colnames(x[["step"]]), c("id_step","desc_step","end_step","module_step","note_step","ref_step"), add = coll)

  checkmate::assert_data_frame(x[["icc"]], ncols = 5, col.names = "named", add = coll)
  checkmate::assert_subset(colnames(x[["icc"]]), c("id_icc","id1","id2","desc1","desc2"), add = coll)

  checkmate::assert_data_frame(x[["outc"]], ncols = 2, col.names = "named", add = coll)
  checkmate::assert_subset(colnames(x[["outc"]]), c("id_outc","desc_outc"), add = coll)

  checkmate::reportAssertions(coll)

  return(x)
}

#' @rdname epicmodel_steplist
#'
#' @export
#'
empty_steplist <- function() {
  x <- vector(mode = "list", length = 8)

  # what
  what_vars <- c("id_what","key_what","desc_what","plural_what")
  x[[1]] <- matrix(c("a0",rep("NA",length(what_vars) - 1)), nrow = 1, ncol = length(what_vars)) %>%
    as.data.frame() %>%
    magrittr::set_colnames(what_vars) %>%
    dplyr::filter(.data$id_what != "a0")
  names(x)[1] <- "what"

  # does
  does_vars <- c("id_does","key_does","subject_singular_does","subject_plural_does","no_subject_does","then_object_does")
  x[[2]] <- matrix(c("d0", rep("NA", length(does_vars) - 1)), nrow = 1, ncol = length(does_vars)) %>%
    as.data.frame() %>%
    magrittr::set_colnames(does_vars) %>%
    dplyr::filter(.data$id_does != "d0")
  names(x)[2] <- "does"

  # where
  where_vars <- c("id_where","key_where","desc_where")
  x[[3]] <- matrix(c("e0",rep("NA",length(where_vars) - 1)), nrow = 1, ncol = length(where_vars)) %>%
    as.data.frame() %>%
    magrittr::set_colnames(where_vars) %>%
    dplyr::filter(.data$id_where != "e0")
  names(x)[3] <- "where"

  # then
  then_vars <- c("id_then","desc_then")
  x[[4]] <- matrix(rep("NA",length(then_vars)), nrow = 1, ncol = length(then_vars)) %>%
    as.data.frame() %>%
    magrittr::set_colnames(then_vars) %>%
    dplyr::filter(.data$id_then != "NA")
  names(x)[4] <- "then"

  # module
  module_vars <- c("id_module","key_module","desc_module")
  x[[5]] <- matrix(c("m0",rep("NA",length(module_vars) - 1)), nrow = 1, ncol = length(module_vars)) %>%
    as.data.frame() %>%
    magrittr::set_colnames(module_vars) %>%
    dplyr::filter(.data$id_module != "m0")
  names(x)[5] <- "module"

  # step
  step_vars <- c("id_step","desc_step","end_step","module_step","note_step","ref_step")
  x[[6]] <- matrix(rep("NA",length(step_vars)), nrow = 1, ncol = length(step_vars)) %>%
    as.data.frame() %>%
    magrittr::set_colnames(step_vars) %>%
    dplyr::filter(.data$id_step != "NA")
  names(x)[6] <- "step"

  # icc (implausible component causes)
  icc_vars <- c("id_icc","id1","id2","desc1","desc2")
  x[[7]] <- matrix(c("i0",rep("NA",length(icc_vars) - 1)), nrow = 1, ncol = length(icc_vars)) %>%
    as.data.frame() %>%
    magrittr::set_colnames(icc_vars) %>%
    dplyr::filter(.data$id_icc != "i0")
  names(x)[7] <- "icc"

  # outc
  outc_vars <- c("id_outc","desc_outc")
  x[[8]] <- matrix(rep("NA",length(outc_vars)), nrow = 1, ncol = length(outc_vars)) %>%
    as.data.frame() %>%
    magrittr::set_colnames(outc_vars) %>%
    dplyr::filter(.data$id_outc != "NA")
  names(x)[8] <- "outc"

  x %<>% new_steplist()
  x %<>% validate_steplist()

  return(x)
}

#' @description `print()` prints a summary of the steplist entries in the console.
#'
#' @method print epicmodel_steplist
#'
#' @rdname epicmodel_steplist
#'
#' @export
#'
print.epicmodel_steplist <- function(x, ...) {
  cli::cli_alert_danger("unchecked (please run {.code check_steplist()} before continuing)")
  cat("WHAT: ",nrow(x$what)," WHAT segments\n")
  cat("DOES: ",nrow(x$does)," DOES segments\n")
  cat("WHERE: ",nrow(x$where)," WHERE segments\n")
  cat("MODULE: ",nrow(x$module)," modules\n")
  cat("STEP: ",nrow(x$step)," STEPs\n")
  if (nrow(x$icc) == 1) {
    cat("ICC: ",nrow(x$icc)," incompatible component-cause pair\n")
  } else {
    cat("ICC: ",nrow(x$icc)," incompatible component-cause pairs\n")
  }
  if (nrow(x$outc) == 1) {
    cat("OUTCOME: ",nrow(x$outc)," outcome definition")
  } else {
    cat("OUTCOME: ",nrow(x$outc)," outcome definitions")
  }
}

#' @method print epicmodel_steplist_checked
#'
#' @rdname epicmodel_steplist
#'
#' @export
#'
print.epicmodel_steplist_checked <- function(x, ...) {
  cli::cli_alert_success("checked successfully")
  cat("WHAT: ",nrow(x$what)," WHAT segments\n")
  cat("DOES: ",nrow(x$does)," DOES segments\n")
  cat("WHERE: ",nrow(x$where)," WHERE segments\n")
  cat("MODULE: ",nrow(x$module)," modules\n")
  cat("STEP: ",nrow(x$step)," STEPs\n")
  if (nrow(x$icc) == 1) {
    cat("ICC: ",nrow(x$icc)," incompatible component-cause pair\n")
  } else {
    cat("ICC: ",nrow(x$icc)," incompatible component-cause pairs\n")
  }
  if (nrow(x$outc) == 1) {
    cat("OUTCOME: ",nrow(x$outc)," outcome definition")
  } else {
    cat("OUTCOME: ",nrow(x$outc)," outcome definitions")
  }
}

#' @method summary epicmodel_steplist
#'
#' @rdname epicmodel_steplist
#'
#' @export
#'
summary.epicmodel_steplist <- function(object, ...) {
  cli::cli_alert_danger("Please first check the steplist using {.fn check_steplist}!")
}

#' @description `summary()` prints a list of steps sorted by type of step in the console.
#'
#' @method summary epicmodel_steplist_checked
#'
#' @param object For `summary.epicmodel_steplist()`, an object of class `epicmodel_steplist`. For `summary.epicmodel_steplist_checked()`, an object
#' of class `epicmodel_steplist_checked`.
#'
#' @rdname epicmodel_steplist
#'
#' @export
#'
summary.epicmodel_steplist_checked <- function(object, ...) {
  cli::cli_h2("Outcome Definitions")
  if (nrow(object$outc) > 0) {
    cli::cli_ul(object$outc$desc_outc)
  } else {
    cli::cli_alert_danger("No outcome definitions")
  }

  prc <- object %>% process_steplist()
  prc_split <- prc %>% split_prc()
  prc %<>% dplyr::full_join(object$then, by = c("then_step" = "id_then"))

  prc_cc <- prc %>% dplyr::filter(.data$id_step %in% prc_split$causes$id_step)
  prc_int <- prc %>% dplyr::filter(.data$id_step %in% prc_split$interventions$id_step)
  prc_end <- prc %>% dplyr::filter(.data$id_step %in% prc_split$end_steps)
  cc_int_end <- c(prc_cc$id_step, prc_int$id_step, prc_end$id_step)
  prc_other <- prc %>% dplyr::filter(!(.data$id_step %in% cc_int_end))

  if (nrow(prc_cc) > 0) {
    cli::cli_h2("Component causes")
    cli::cli_ul(prc_cc$desc_then)
  }

  if (nrow(prc_int) > 0) {
    cli::cli_h2("Interventions")
    cli::cli_ul(prc_int$desc_then)
  }

  if (nrow(prc_end) > 0) {
    cli::cli_h2("End steps")
    cli::cli_ul(prc_end$desc_then)
  }

  if (nrow(prc_other) > 0) {
    cli::cli_h2("Other steps")
    cli::cli_ul(prc_other$desc_then)
  }
}

#' @method plot epicmodel_steplist
#'
#' @rdname epicmodel_steplist
#'
#' @export
#'
plot.epicmodel_steplist <- function(x, ...) {
  cli::cli_alert_danger("Please first check the steplist using {.fn check_steplist}!")
}

#' @description `plot()` renders a graph of the complete network of mechanisms in the RStudio Viewer.
#'
#' @method plot epicmodel_steplist_checked
#'
#' @param modules For `plot.epicmodel_steplist_checked`, TRUE (default) or FALSE, indicating if nodes in the same module should be colored equally
#' (TRUE) or if all nodes have white background (FALSE). Colors are only applied, if modules have actually been specified in the
#' `epicmodel_steplist`.
#' @param module_colors For `plot.epicmodel_steplist_checked`, if nodes are colored by module, colors can be provided via this argument. Colors
#' must be provided as a character vector. Both named colors and hexadecimal color codes are allowed. The function has 8 colors stored internally.
#' If `module_colors` = NULL (default), these colors are used. If the model has more than 8 modules, `module_colors` must be specified. If more
#' colors than necessary are specified, the function takes as many as necessary from the start of the vector.
#' @param render For `plot.epicmodel_steplist_checked`, if TRUE (default), graph is directly rendered. IF FALSE, the output contains the
#' non-rendered graph.
#'
#' @rdname epicmodel_steplist
#'
#' @export
#'
plot.epicmodel_steplist_checked <- function(x, modules = TRUE, module_colors = NULL, render = TRUE, ...) {
  # Check input
  rlang::try_fetch(checkmate::assert_logical(modules, any.missing = F, len = 1, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var modules} must be TRUE or FALSE!",
                                    parent = cnd, class = "input_modules")
                   })

  rlang::try_fetch(checkmate::assert_logical(render, any.missing = F, len = 1, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var render} must be TRUE or FALSE!",
                                    parent = cnd, class = "input_render")
                   })

  rlang::try_fetch(checkmate::assert_character(module_colors, any.missing = F, min.len = 1, min.chars = 1, null.ok = T),
                   error = function(cnd) {
                     cli::cli_abort("{.var module_colors} must be a character vector holding valid color descriptions!",
                                    parent = cnd, class = "input_module_colors")
                   })

  if (!is.null(module_colors)) {
    if (!are_colors(module_colors)) {
      cli::cli_abort("{.var module_colors} contains invalid color descriptions!", class = "invalid_module_colors")
    }
  }
  #=============================================================================
  # Process steplist
  steplist <- x
  prc <- steplist %>% process_steplist()
  prc_split <- prc %>% split_prc()
  ## Add THEN description
  prc %<>% dplyr::full_join(steplist$then, by = c("then_step" = "id_then"))

  # Get module usage
  use_modules <- are_modules_used(steplist)

  # Add module colors
  if (use_modules & modules) {
    ## Get number of modules
    n_modules <- nrow(steplist$module)
    ## No module_colors specified
    if (is.null(module_colors) & n_modules > 8) {
      cli::cli_abort(c("Your steplist contains more than 8 modules. This function can only color up to 8 modules without specification of
                       {.var module_colors}.",
                       "i" = "Set {.var modules} to FALSE or provide a character vector with at least {n_modules} colors for {.var module_colors}."))
    }
    if (is.null(module_colors) & n_modules <= 8) {
      colors_used <- c("#fbb4ae","#b3cde3","#ccebc5","#ffffcc","#fddaec","#fed9a6","#cccccc","#e5d8bd")[1:n_modules]
    }
    ## module_colors specified
    if (!is.null(module_colors)) {
      if (length(module_colors) < n_modules) {
        cli::cli_abort("Your steplist contains {n_modules} modules, but you only specified {length(module_colors)} colors in {.var module_colors}.")
      } else {
        colors_used <- module_colors[1:n_modules]
      }
    }
    ## Merge color to prc
    color_df <- steplist$module %>% cbind(.,colors_used)
    prc %<>% dplyr::left_join(color_df, by = c("module_step" = "id_module"))

  } else {
    ## Only warn if "modules = T" has been included in the function call
    if (methods::hasArg(modules) & modules) {warning("No modules have been specified!")}
    prc$colors_used <- "white"
  }

  # Add node label
  prc$label <- NA_character_
  ## Split into parts by step type
  prc_cc <- prc %>% dplyr::filter(.data$id_step %in% prc_split$causes$id_step)
  prc_int <- prc %>% dplyr::filter(.data$id_step %in% prc_split$interventions$id_step)
  prc_end <- prc %>% dplyr::filter(.data$id_step %in% prc_split$end_steps)
  cc_int_end <- c(prc_cc$id_step, prc_int$id_step, prc_end$id_step)
  prc_other <- prc %>% dplyr::filter(!(.data$id_step %in% cc_int_end))
  ## Add labels by step type
  if (nrow(prc_cc) > 0) {prc_cc$label <- paste0("CC",c(1:nrow(prc_cc)))}
  if (nrow(prc_int) > 0) {prc_int$label <- paste0("I",c(1:nrow(prc_int)))}
  if (nrow(prc_end) > 0) {prc_end$label <- paste0("E",c(1:nrow(prc_end)))}
  if (nrow(prc_other) > 0) {prc_other$label <- paste0("S",c(1:nrow(prc_other)))}

  # Nodes
  ndf_cc <- DiagrammeR::create_node_df(n = nrow(prc_cc),
                                       type = "cc",
                                       label = prc_cc$label,
                                       tooltip = prc_cc$desc_then,
                                       shape = "square",
                                       color = "gray",
                                       fillcolor = prc_cc$colors_used,
                                       fontcolor = "black")
  ndf_int <- DiagrammeR::create_node_df(n = nrow(prc_int),
                                        type = "int",
                                        label = prc_int$label,
                                        tooltip = prc_int$desc_then,
                                        shape = "triangle",
                                        color = "gray",
                                        fillcolor = prc_int$colors_used,
                                        fontcolor = "black")
  ndf_end <- DiagrammeR::create_node_df(n = nrow(prc_end),
                                        type = "end",
                                        label = prc_end$label,
                                        tooltip = prc_end$desc_then,
                                        shape = "circle",
                                        color = "black",
                                        fillcolor = prc_end$colors_used,
                                        fontcolor = "black")
  ndf_other <- DiagrammeR::create_node_df(n = nrow(prc_other),
                                          type = "other",
                                          label = prc_other$label,
                                          tooltip = prc_other$desc_then,
                                          shape = "circle",
                                          color = "gray",
                                          fillcolor = prc_other$colors_used,
                                          fontcolor = "black")
  ndf <- DiagrammeR::combine_ndfs(ndf_cc, ndf_int, ndf_end, ndf_other)

  # Edges
  ## Merge node ids to prc_temp
  prc <- rbind(prc_cc, prc_int, prc_other, prc_end)
  prc %<>% dplyr::left_join(ndf[,c("label","id")], by = "label")

  ## If
  ### Get dataset of steps in the current sufficient cause with IF conditions
  if_temp <- prc %>% dplyr::filter(!is.na(.data$if_step))
  if (nrow(if_temp) > 0) {
    ### Create empty container
    if_edges <- vector(mode = "list", length = nrow(if_temp))
    ### Loop over all steps with IF condition
    for (j in 1:length(if_edges)) {
      #### Get if_list from processed steplist
      if_list_temp <- if_temp$if_list[[j]]
      #### Get number of entries in if_list
      n_if_list_temp <- nrow(if_list_temp)
      #### Create output container for current step (from is set to this steps node id)
      out_temp <- data.frame(from = rep(NA_integer_, n_if_list_temp),
                             to = rep(if_temp$id[j], n_if_list_temp),
                             sce = rep(NA_character_, n_if_list_temp))
      #### Loop over every line in if_list
      for (k in 1:n_if_list_temp) {
        ##### Check if the current part of the IF condition has been used for the current sufficient cause
        if ((prc$then_step == if_list_temp$id[k]) %>% all_false()) {
          ###### If not, set "from" from 0 instead of the node id
          out_temp$from[k] <- 0
        } else {
          ###### If yes, set "from" from the corresponding node id
          out_temp$from[k] <- prc$id[prc$then_step == if_list_temp$id[k]]
        }
        ##### Record scenario as well
        out_temp$sce[k] <- if_list_temp$sce[k]
      }
      #### If there are scenarios with parts set from 0, all other parts are set from 0 as well,
      #### thereby only completed IF conditions appear in the graph
      if ((out_temp$from == 0) %>% all_false() %>% magrittr::not()) {
        to_zero <- out_temp$sce[out_temp$from == 0]
        out_temp$from[out_temp$sce %in% to_zero] <- 0
      }
      if_edges[[j]] <- out_temp
    }

    if_edges %<>% purrr::map_dfr(as.data.frame) %>% dplyr::filter(.data$from != 0)
    edf_if <- DiagrammeR::create_edge_df(from = if_edges$from,
                                         to = if_edges$to,
                                         rel = "if",
                                         arrowhead = "normal",
                                         arrowsize = 1,
                                         color = "gray")
    edf_if$dupli <- edf_if[,c("from","to")] %>% duplicated()
    edf_if %<>% dplyr::filter(.data$dupli == FALSE)
    edf_if$dupli <- NULL
  }

  ## Ifnot
  ### Get dataset of steps in the current sufficient cause with IFNOT conditions
  ifnot_temp <- prc %>% dplyr::filter(!is.na(.data$ifnot_step))
  if (nrow(ifnot_temp) > 0) {
    ### Create empty container
    ifnot_edges <- vector(mode = "list", length = nrow(ifnot_temp))
    ### Loop over all steps with IF condition
    for (j in 1:length(ifnot_edges)) {
      #### Get ifnot_list from processed steplist
      ifnot_list_temp <- ifnot_temp$ifnot_list[[j]]
      #### Get number of entries in ifnot_list
      n_ifnot_list_temp <- nrow(ifnot_list_temp)
      #### Create output container for current step (from is set to this steps node id)
      out_temp <- data.frame(from = rep(NA_integer_, n_ifnot_list_temp),
                             to = rep(ifnot_temp$id[j], n_ifnot_list_temp),
                             sce = rep(NA_character_, n_ifnot_list_temp))
      #### Loop over every line in ifnot_list
      for (k in 1:n_ifnot_list_temp) {
        ##### Check if the current part of the IF condition has been used for the current sufficient cause
        if ((prc$then_step == ifnot_list_temp$id[k]) %>% all_false()) {
          ###### If not, set "from" from 0 instead of the node id
          out_temp$from[k] <- 0
        } else {
          ###### If yes, set "from" from the corresponding node id
          out_temp$from[k] <- prc$id[prc$then_step == ifnot_list_temp$id[k]]
        }
        ##### Record scenario as well
        out_temp$sce[k] <- ifnot_list_temp$sce[k]
      }
      #### If there are scenarios with parts set from 0, all other parts are set from 0 as well,
      #### thereby only completed IF conditions appear in the graph
      if ((out_temp$from == 0) %>% all_false() %>% magrittr::not()) {
        to_zero <- out_temp$sce[out_temp$from == 0]
        out_temp$from[out_temp$sce %in% to_zero] <- 0
      }
      ifnot_edges[[j]] <- out_temp
    }
      ifnot_edges %<>% purrr::map_dfr(as.data.frame) %>% dplyr::filter(.data$from != 0)
      edf_ifnot <- DiagrammeR::create_edge_df(from = ifnot_edges$from,
                                            to = ifnot_edges$to,
                                            rel = "ifnot",
                                            arrowhead = "tee",
                                            arrowsize = 1.2,
                                            color = "#A65141")
      edf_ifnot$dupli <- edf_ifnot[,c("from","to")] %>% duplicated()
      edf_ifnot %<>% dplyr::filter(.data$dupli == FALSE)
      edf_ifnot$dupli <- NULL
  }

  if (nrow(if_temp) == 0 & nrow(ifnot_temp) > 0) {
    edf <- edf_ifnot
  }
  if (nrow(if_temp) > 0 & nrow(ifnot_temp) == 0) {
    edf <- edf_if
  }
  if (nrow(if_temp) > 0 & nrow(ifnot_temp) > 0) {
    edf <- DiagrammeR::combine_edfs(edf_if, edf_ifnot)
  }

  # Create legend
  if (use_modules & modules) {
    legend <- prc %>% dplyr::select(dplyr::all_of(c("label","key_module","desc_then"))) %>% as.data.frame()
    colnames(legend)[which(colnames(legend) == "label")] <- "Label"
    colnames(legend)[which(colnames(legend) == "key_module")] <- "Module"
    colnames(legend)[which(colnames(legend) == "desc_then")] <- "Step"
  } else {
    legend <- prc %>% dplyr::select(dplyr::all_of(c("label","desc_then"))) %>% as.data.frame()
    colnames(legend)[which(colnames(legend) == "label")] <- "Label"
    colnames(legend)[which(colnames(legend) == "desc_then")] <- "Step"
  }

  # Create graph
  gr <- DiagrammeR::create_graph(nodes_df = ndf, edges_df = edf, attr_theme = "lr")

  # Render
  if (render) {
    gr %<>% DiagrammeR::render_graph()
  }

  # Return
  return(list(Graph = gr, Legend = legend))
}
