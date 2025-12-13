#' Explore effect of prevention
#'
#' Prevention refers to the avoidance of component causes, i.e., of elements of sufficient causes. For a given set of component causes, `prevent()`
#' derives, which of them need to be "removed" in order to avoid outcome occurrence. Reported are the smallest prevention sets, i.e., with the
#' fewest component causes.
#'
#' @details The following algorithm is used to evaluate the effect of prevention:
#'  * Evaluate if `causes` is sufficient for outcome occurrence. If not, report so and stop.
#'  * Derive a list of all combinations of the component causes provided in `causes`. The set "all causes present" is not evaluated as it is
#'    already known to be sufficient. In addition, the set "all causes absent", i.e., "all causes prevented" is considered.
#'  * Evaluate sufficiency for every set
#'  * Subset the list of cause sets to the ones, which are `not` sufficient, because for them prevention was successful.
#'  * Turn all FALSE to TRUE and all TRUE to FALSE. Now, FALSE indicates present and TRUE indicates absent, i.e., prevented.
#'  * Evaluate, which prevention sets are minimal, i.e., the smallest set to prevent the outcome.
#'
#' @param scc An object of class `epicmodel_scc`.
#' @param causes A character vector containing step IDs of component causes. If NULL (default), prints a list of all available component causes in
#' the console.
#' @param output Either "nice" (default) or "table". If "nice", prints a nicely formatted summary in the console. If "table", returns a data.frame
#' (described in section "Value" below).
#'
#' @returns If `output = "nice"` (default), prints a nicely formatted output in the console. If `output = "table"`, returns a data.frame with one
#' row for every prevention set and one column for every component cause provided in argument `causes`. All cells are either TRUE or FALSE with TRUE
#' indicating that the corresponding variable needs to be prevented in the corresponding set, and FALSE indicating that prevention in the corresponding
#' set is not necessary.
#'
#' @export
#'
#' @examples
#' # Derive SCC model
#' scc_model <- scc_rain
#'
#' # Derive prevention sets
#' prevent(scc_model, causes = c("IFNOTd6a6THENd5a6","THENa5","THENa1","THENd2a3"))
prevent <- function(scc, causes = NULL, output = c("nice", "table")) {
  # Check input
  if (inherits(scc, "epicmodel_scc") %>% magrittr::not()) {
    cli::cli_abort("{.var scc} must be a {.emph epicmodel_scc} class object!",
                   class = "no_scc")
  }

  rlang::try_fetch({
    checkmate::assert_character(causes, any.missing = F, null.ok = T, min.len = 1, min.chars = 1)
  }, error = function(cnd) {cli::cli_abort(c("{.var causes} must be a character vector consisting of step IDs of component causes!",
                                             "i" = "Use 'all' to select all sufficient causes, i.e., all minimally sufficient sets of component
                                                      causes.",
                                             "i" = "To print a list of all available component causes in the console, do not specify argument
                                                      {.var causes}."),
                                           parent = cnd, class = "input_causes")
  })

  rlang::try_fetch(output <- match.arg(output),
                   error = function(cnd) {cli::cli_abort("{.var output} must be one of the following strings: 'nice' or 'table'!",
                                                         parent = cnd, class = "input_output")
                   })
  #=============================================================================
  # Process steplist
  prc <- scc$steplist %>% process_steplist()
  split <- prc %>% split_prc()

  # Get causes
  cause_set <- get_causes_prevent(scc = scc, causes = causes, split = split)

  if (!is.null(cause_set) & !is.null(causes)) {
    # Prepare container
    prev_suff <- rep(NA, nrow(cause_set)) %>% magrittr::set_names(rownames(cause_set))

    # Get rowname of row with all values equal to FALSE
    all_prev_rowname <- rownames(cause_set)[cause_set %>% rowSums() %>% magrittr::equals(0)]
    if (all_prev_rowname %>% length() %>% magrittr::equals(1) %>% magrittr::not()) {
      cli::cli_abort(cli::cli_abort("Internal error `prevent1`", .internal = TRUE,
                                    class = "error_prevent1"))
    }

    # Get sufficiency status
    for (i in 1:nrow(cause_set)) {
      if (rownames(cause_set)[i] == all_prev_rowname) {
        prev_suff[[rownames(cause_set)[i]]] <- FALSE
      } else {
        ## Transform to character vector
        cause_set_vec <- colnames(cause_set)[cause_set[i,] %>% t() %>% magrittr::extract(,1)]
        ## Get sufficiency status without intervention
        prev_suff[[rownames(cause_set)[i]]] <- scc %>% are_sufficient(causes = cause_set_vec, type = "binary")
      }
    }

    # Subset cause_set to all non-suffcient sets
    cause_set %<>% dplyr::filter(rownames(cause_set) %in% names(prev_suff)[!prev_suff])

    # Switch TRUE and FALSE in the new cause_set to mark prevented variables as TRUE
    prev_set <- cause_set %>% magrittr::not() %>% as.data.frame()

    # Minimize set of prevented component causes
    prev_set %<>% minimize_prev()

    # Return
    if (output == "table") {
      return(prev_set)
    }
    if (output == "nice") {
      nice_output_prevent(prev_set, scc, split)
    }
  }
}

#' Get sets of component causes for `prevent()`
#'
#' Used in `prevent()`.
#'
#' @param scc An object of class `epicmodel_scc`.
#' @param causes The argument `causes` from `prevent()`, which is a character vector with at least one element or NULL (default in
#' `prevent()` but not in `get_causes_prevent()`). Allowed elements are step IDs of causes. If NULL, prints a list of all available causes in
#' the model into the console.
#' @param split A list of sets of steps from `scc` created by `process_steplist()` and `split_prc()`.
#'
#' @returns If `causes` is NULL, prints a list of available causes into the console. Otherwise, returns a data.frame with columns equal to the
#' number of causes and one row for every set of causes that will be investigated, i.e., one row for every combination of component causes,
#' named with `prev1`, `prev2`, etc. In contrast to the list returned by `get_cause_combinations()`, the data.frame does not contain the
#' combination of all specified causes, because it is already know that it is sufficient to cause the outcome. Instead, the set with all causes
#' absent is added and refers to all causes in the specified set being prevented. Cell values are either TRUE or FALSE indicating if the
#' corresponding cause is present, i.e., not prevented (TRUE), or absent, i.e., prevented (FALSE). If the specified set of causes is not
#' sufficient, returns an information alert to inform the user about it.
#'
#' @noRd
get_causes_prevent <- function(scc, causes, split) {
  # Check input
  if (inherits(scc, "epicmodel_scc") %>% magrittr::not()) {
    cli::cli_abort("Input validation error: {.var scc}", .internal = TRUE, class = "input_scc")
  }

  rlang::try_fetch({
      checkmate::assert_character(causes, any.missing = F, null.ok = T, min.len = 1, min.chars = 1)
    }, error = function(cnd) {cli::cli_abort("Input validation error: {.var causes}", .internal = TRUE,
                                             parent = cnd, class = "input_causes")
  })

  rlang::try_fetch({
      checkmate::assert_list(split, any.missing = F, null.ok = F, len = 5, names = "unique")
      checkmate::assert_subset(names(split), c("non_start_steps","causes","interventions","ifnot_steps","end_steps"), empty.ok = F)
    }, error = function(cnd) {cli::cli_abort("Input validation error: {.var split}", .internal = TRUE,
                                             parent = cnd, class = "input_split")
  })
  #=============================================================================
  # With no causes specified
  if (is.null(causes)) {
    ## Get list of causes and combine ID and description
    list_of_steps <- split$causes %>% dplyr::left_join(scc$steplist$step[,c("id_step","desc_step")], by = "id_step") %>%
      dplyr::select(dplyr::all_of(c("id_step","desc_step")))
    list_of_steps$merge <- paste0(list_of_steps$id_step,": ",list_of_steps$desc_step %>% stringr::str_replace("Start: ",""))
    ## Return list of causes
    return({
      cli::cli_h2("Component Causes")
      cli::cli_ul(list_of_steps$merge)
    })
  } else {
  # With causes specified
    ## Check if selected causes exist
    if (causes %>% magrittr::is_in(split$causes$id_step) %>% all() %>% magrittr::not()) {
      invalid_causes <- causes[causes %>% magrittr::is_in(split$causes$id_step) %>% magrittr::not()] %>%
        stringr::str_c(collapse = ", ")
      cli::cli_abort(c("All elements of {.var causes} must be IDs of valid component causes!",
                       "i" = "The following component causes are invalid: {invalid_causes}",
                       "i" = "Run {.fn prevent} without {.var causes} argument to list all available component causes."),
                     class = "invalid_causes")
    } else {
      ## Check if causes are sufficient
      if (scc %>% are_sufficient(causes, type = "binary") %>% magrittr::not()) {
        cli::cli_alert_info("The specified set of component causes is not sufficient!")
        return(invisible(NULL))
      }

      ## Check for incompatible component causes
      if (nrow(scc$steplist$icc) > 0) {
        icc_check <- scc$steplist$icc
        icc_check$check <- FALSE
        for (i in 1:nrow(icc_check)) {
            if ((icc_check[i,"id1"] %in% causes) & (icc_check[i,"id2"] %in% causes)) {
              icc_check$check[i] <- TRUE
            }
        }
        if (icc_check$check %>% any()) {
          icc_check$combi_desc <- paste(icc_check$desc1," <> ",icc_check$desc2)
          icc_causes <- icc_check %>% dplyr::filter(.data$check == TRUE) %>% magrittr::extract2("combi_desc") %>%
            stringr::str_c(collapse = ", ")
          cli::cli_abort(c("{.var causes} contains incompatible component causes!",
                           "i" = "The following component causes are incompatible: {icc_causes}"),
                         class = "icc_causes")
        }
      }

      ## Get cause_set
      cause_set <- split$causes %>% dplyr::filter(.data$id_step %in% causes) %>% get_cause_combinations(scc$steplist)
      ### Switching TRUE and FALSE as an easy way to remove the row with all values being TRUE and adding the row with all value being FALSE
      ### TRUE still means present and FALSE still means absent or prevented
      cause_set %<>% magrittr::not() %>% as.data.frame() %>% magrittr::set_rownames(paste0("prev",c(1:nrow(cause_set))))
    }
  }
  #=============================================================================
  # Check output
  rlang::try_fetch({
      checkmate::assert_data_frame(cause_set, any.missing = F, null.ok = F, types = "logical", min.rows = 1, min.cols = 1, row.names = "unique",
                                   col.names = "unique")
      checkmate::assert_character(rownames(cause_set), pattern = "^prev[[:digit:]]+$")
      checkmate::assert_subset(colnames(cause_set), choices = split$causes$id_step, empty.ok = F)
    }, error = function(cnd) {cli::cli_abort("Output validation error", .internal = TRUE,
                                             parent = cnd, class = "output")
  })
  #=============================================================================
  return(cause_set)
}

#' Minimize list of causes to prevent
#'
#' Select smallest prevention sets. Used in `prevent()`.
#'
#' @details Only reports the smallest sets of component causes to prevent. A smaller set contains no additional component causes compared to the
#' larger set, but the larger set contains some component cause that are not part of the smaller set. Therefore, minimality is checked via two
#' criteria:
#' * Are all un-prevented component causes from set 1 also un-prevented in set 2?
#' * Does set 2 needs to prevent less component causes than set 1?
#' If both is TRUE, set 2 is smaller than set 1, and therefore, set 1 is not minimal, since at least one smaller set exists. In the code, set 1
#' corresponds to the i-th set and set 2 corresponds to the j-th set. If the j-th set fulfills both criteria, minimality for the i-th set is changed
#' to FALSE.
#'
#' @param prev_set A data.frame with colnames equal to some step IDs of the component causes and rownames of format `^prev[[:digit:]]+$`. Contains
#' only TRUE or FALSE and no missings.
#'
#' @returns An object similar to input `prev_set`, but with less rows. (If all sets are minimal, returns exactly input `prev_set`.)
#'
#' @noRd
minimize_prev <- function(prev_set) {
  # Check input
  rlang::try_fetch({
      checkmate::assert_data_frame(prev_set, types = "logical", any.missing = F, null.ok = F, min.cols = 1, min.rows = 1,
                                   col.names = "unique", row.names = "unique")
      checkmate::assert_character(rownames(prev_set), pattern = "^prev[[:digit:]]+$")
    }, error = function(cnd) {cli::cli_abort("Input validation error: {.var prev_set}", .internal = TRUE,
                                             parent = cnd, class = "input_prev_set")
  })
  #=============================================================================
  # Get number of sufficient sets of component causes
  n_rows <- nrow(prev_set)

  # Create empty container
  minimal <- rep(TRUE, n_rows) %>% magrittr::set_names(rownames(prev_set))

  # Loop over all sufficient sets of component causes
  cli::cli_progress_bar("Check if prevention set is minimal", total = n_rows, type = "tasks")
  for (i in 1:n_rows) {
    ## Which component causes are FALSE for the i-th set
    left_out_start <- colnames(prev_set)[which(prev_set[i, ] == F)]
    ## Loop over all sufficient sets of component causes (to compare with the i-th set)
    for (j in 1:n_rows) {
      ### Check if the component causes that are missing in the i-th set, are also missing in the j-th set
      if (length(left_out_start) > 0) {
        also_false <- prev_set[j,left_out_start] %>% as.logical() %>% any() %>% magrittr::not()
      } else {
        also_false <- T
      }
      ### Check if the j-th set contains less component causes than the i-th set
      less_starts <- prev_set[j,] %>% sum() %>% magrittr::is_less_than(prev_set[i, ] %>% sum())
      ### If both is TRUE for any j-th set, the i-th set is not minimal, i.e., at least on of the j-th sets is smaller
      if (also_false & less_starts) {
        minimal[rownames(prev_set)[i]] <- FALSE
      }
    }
    cli::cli_progress_update()
  }

  # Subset prev_set to minimal sets
  # LEGACY: prev_set <- prev_set[minimal,]
  prev_set %<>% dplyr::filter(rownames(prev_set) %in% names(minimal)[minimal])
  #=============================================================================
  # Check output
  rlang::try_fetch({
      checkmate::assert_data_frame(prev_set, types = "logical", any.missing = F, null.ok = F, min.cols = 1, min.rows = 1,
                                   col.names = "unique", row.names = "unique")
      checkmate::assert_character(rownames(prev_set), pattern = "^prev[[:digit:]]+$")
    }, error = function(cnd) {cli::cli_abort("Output validation error: {.var prev_set}", .internal = TRUE,
                                             parent = cnd, class = "output_prev_set")
  })
  #=============================================================================
  cli::cli_alert_success(paste0(n_rows,"/",n_rows," | Check if prevention set is minimal"))
  cli::cli_alert_info(paste0(nrow(prev_set),"/",n_rows," prevention sets are minimal"))
  return(prev_set)
}

#' Create nice output for `prevent()`
#'
#' Used in `prevent()` if `output = "nice"`.
#'
#' @param prev_set The output of `prevent()`, if `output = "table"`.
#' @param scc Argument `scc` from the corresponding call of `prevent()`.
#' @param split A list of sets of steps from `scc` created by `process_steplist()` and `split_prc()`.
#'
#' @returns Nothing, but prints a nicely formatted version of `prev_set` into the console.
#'
#' @noRd
nice_output_prevent <- function(prev_set, scc, split) {
  # Merge THEN description to processed steplist
  split$causes %<>% dplyr::left_join(scc$steplist$then, by = c("then_step" = "id_then"))

  # Start with headline
  cli::cli_h1("Prevention")

  # Print cause set
  cli::cli_h2("Cause set")
  cli::cli_text("Individuals with exactly the following component causes:")
  cli::cli_ul(split$causes %>% dplyr::filter(.data$id_step %in% colnames(prev_set)) %>%
    magrittr::extract2("desc_then"))

  # Print prevention sets
  cli::cli_h2("Prevention sets")
  cli::cli_text("Prevent the outcome by preventing any of the following sets:")

  for (i in 1:nrow(prev_set)) {
    ## Transform to character vector
    prev_set_vec <- colnames(prev_set)[prev_set[i,] %>% t() %>% magrittr::extract(,1)]
    ## Get descriptions
    prev_set_desc <- split$causes %>% dplyr::filter(.data$id_step %in% prev_set_vec) %>%
      magrittr::extract2("desc_then")

    cli::cli_h3(paste("Set ",i))
    cli::cli_par()
    cli::cli_ul(prev_set_desc)
    cli::cli_end()
  }
}
