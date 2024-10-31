#' Explore effect of interventions
#'
#' Interventions are steps without IF condition (start steps) that only appear in other IFNOT conditions, i.e., that can only prevent steps but not
#' cause them. Interventions are not considered when creating SCC models using [create_scc()]. `intervene()` evaluates their impact in two
#' directions: 1) which sufficient causes can be prevented by certain (sets of) interventions and 2) which set of interventions is at least needed
#' to prevent the outcome in an individual with a given set of component causes.
#'
#' @details The following algorithm is used to evaluate the effect of interventions:
#'  * Derive the list of intervention sets to evaluate
#'  * Derive the list of sets of component causes to evaluate
#'  * Evaluate sufficiency without intervention for every set of component causes
#'  * Evaluate sufficiency for every combination of intervention set and set of componen causes: First, check which steps are prevented by the
#'    corresponding set of interventions, i.e., for which steps the IFNOT condition is fulfilled by the intervention set. These steps are removed
#'    from the list of available steps. Second, evaluate sufficiency based on the remaining steps similar to [create_scc()] (Check sufficiency &
#'    Check IFNOT conditions).
#'  * Evaluate, which intervention sets are minimal, i.e., at least necessary to prevent the outcome
#'
#' @param scc An object of class `epicmodel_scc`.
#' @param causes A character vector containing step IDs of component causes. If "all", investigates all sufficient causes, i.e., all minimally
#' sufficient sets of component causes. If NULL (default), prints a list of all available component causes in the console. If a set of step IDs is
#' specified, only the specified set is investigated.
#' @param intervention A character vector containing step IDs of interventions. If "all", investigates all possible combinations of available
#' interventions. If NULL (default), prints a list of all available interventions in the console. If a set of step IDs is specified, investigates
#' all possible combinations of the specified interventions.
#' @param output Either "nice" (default) or "table". If "nice", prints a nicely formatted summary in the console. If "table", returns a list of
#' several elements described in detail in section "Value" below.
#'
#' @returns ## Output
#' If `output = "nice"` (default), prints a nicely formatted output in the console. If `output = "table"`, returns a list with the following
#' elements:
#' \describe{
#'  \item{`cause_set`}{A list of character vectors with one element for every investigated set of component causes. The character vectors contain the
#'    step IDs of the component causes that are part of the corresponding set. Sets are named in a format similar to cc1, cc2, etc.}
#'
#'  \item{`intv`}{A list of character vectors with one element for every investigated set of interventions. The character vectors contain the step IDs of
#'    the interventions that are part of the corresponding set. Sets are named as intv1, intv2, etc.}
#'
#'  \item{`status`}{A data.frame with one row per set of component causes and one column per set of intervention. In addition, contains one column
#'    representing no interventions (`intv0`). Each cell contains the sufficiency status of the corresponding set of component causes when the
#'    corresponding set of interventions is applied. Possible values are "always", "depends", and "never". See below for an interpretation.}
#'
#'  \item{`minimal`}{A data.frame with one row per set of component causes and one column per set of intervention. Each cell is either TRUE or FALSE
#'    indicating if the set of interventions is minimal. For non-minimal sets of interventions, a smaller set which is contained within the
#'    corresponding set exists and has the same preventive power. Minimality is defined separately for every set of component causes. If both the
#'    larger non-minimal and the smaller minimal set sometimes prevent the outcome (status "depends" in `status` (see above)), the non-minimal set
#'    might actually prevent more sufficient orders of occurrence than the minimal set. In this case, please inspect and compare element `order`
#'    (see next), for all minimal and non-minimal sets of interventions with status "depends".}
#'
#'  \item{`order`}{A 2-level list, i.e., a list with one element per intervention set, for which each element is another list with one element per
#'    evaluated set of component causes. Each intervention/component causes combination contains a data.frame, similar to the data.frames in the
#'    `sc_order` element of `epicmodel_scc` objects, if the corresponding status is "depends", or is NA otherwise (for "always" or
#'    "never"). The data.frames contain two columns, which are called "order" and "suff" (short for "sufficient"), and one row for every order of
#'    occurrence. The order of occurrence is summarized in "order" (as character), while "suff" is either TRUE or FALSE indicating if the
#'    corresponding order of occurrence is sufficient, i.e., leads to the outcome, or not. Please note that the prevented orders of occurrence
#'    have `suff == FALSE`.}
#'  }
#'
#'  ## How to interpret `status`
#'
#'  If the sufficiency status for a certain intervention in column `intv0` is `always`, the three sufficiency status options for a certain
#'  intervention have the following interpretations:
#'  * `always`: The corresponding set of inteventions never prevents the outcome, because after applying the intervention, the corresponding set of
#'     component causes is still always sufficient.
#'  * `depends`: The corresponding set of interventions sometimes prevents the outcome, because after applying the intervention, sufficiency for the
#'    corresponding set of component causes depends on the order of occurrence.
#'  * `never`: The corresponding set of interventions always prevents the outcome, because after applying the intervention, the corresponding set of
#'    component causes is never sufficient.
#'
#'  If the sufficiency status for a certain intervention in column `intv0` is `depends`, the sufficiency status options for a certain
#'  intervention have the following interpretations:
#'  * `depends`: The corresponding set of interventions sometimes or never prevents the outcome, because after applying the intervention,
#'    sufficiency for the corresponding set of component causes depends on the order of occurrence. Further inspection and comparison
#'    of sufficient orders of occurrence is necessary to determine if the intervention actually prevents anything.
#'  * `never`: The corresponding set of interventions always prevents the outcome, because after applying the intervention, the corresponding set of
#'    component causes is never sufficient.
#'
#'  If the sufficiency status for a certain intervention in column `intv0` is `never`, no intervention is necessary, because the corresponding set of
#'  component causes is never sufficient.
#'
#' @export
#'
#' @examples
#' # Create some SCC model that contains interventions
#' steplist_checked <- check_steplist(steplist_rain)
#' scc_model <- create_scc(steplist_checked)
#'
#' # Inspect the effect of interventions
#' intervene(scc_model, causes = "all", intervention = "all")
#' intv <- intervene(scc_model, causes = "all", intervention = "all", output = "table")
intervene <- function(scc, causes = NULL, intervention = NULL, output = c("nice", "table")) {
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

  rlang::try_fetch({
      checkmate::assert_character(intervention, any.missing = F, null.ok = T, min.len = 1, min.chars = 1)
    }, error = function(cnd) {cli::cli_abort(c("{.var intervention} must be a character vector consisting of step IDs of interventions!",
                                               "i" = "Use 'all' to select all possible combinations of available interventions.",
                                               "i" = "To print a list of all available interventions in the console, do not specify argument
                                                      {.var intervention}."),
                                             parent = cnd, class = "input_intervention")
  })

  rlang::try_fetch(output <- match.arg(output),
                   error = function(cnd) {cli::cli_abort("{.var output} must be one of the following strings: 'nice' or 'table'!",
                                                         parent = cnd, class = "input_output")
                   })
  #=============================================================================
  # Process steplist
  prc <- scc$steplist %>% process_steplist()
  split <- prc %>% split_prc()

  # Prepare outc_list
  outc_list <- scc$steplist %>% transform_outc()

  # Get interventions
  intv <- get_intv(scc = scc, intervention = intervention, split = split)

  # Get causes
  cause_set <- get_causes(scc = scc, causes = causes, split = split)

  if (!is.null(causes) & !is.null(intervention)) {
    # Prepare container
    out_status <- matrix(rep(NA_character_, nrow(cause_set) * (nrow(intv) + 1)), nrow = nrow(cause_set), ncol = nrow(intv) + 1) %>%
      as.data.frame() %>% magrittr::set_rownames(rownames(cause_set)) %>% magrittr::set_colnames(c("intv0",rownames(intv)))
    out_intv <- vector(mode = "list", length = nrow(intv)) %>% magrittr::set_names(rownames(intv))
    out_cause_set <- vector(mode = "list", length = nrow(cause_set)) %>% magrittr::set_names(rownames(cause_set))
    out_order <- vector(mode = "list", length = nrow(intv)) %>% magrittr::set_names(rownames(intv))
    for (i in 1:length(out_order)) {
      out_order[[i]] <- vector(mode = "list", length = nrow(cause_set)) %>% magrittr::set_names(rownames(cause_set))
    }

    # Get status without intervention (also save character for output)
    for (i in 1:nrow(cause_set)) {
      ## Transform to character vector
      cause_set_vec <- colnames(cause_set)[cause_set[i,] %>% t() %>% magrittr::extract(,1)]
      ## Save character vector for output
      out_cause_set[[rownames(cause_set)[i]]] <- cause_set_vec
      ## Get sufficiency status without intervention
      out_status[rownames(cause_set)[i],"intv0"] <- scc %>% are_sufficient(causes = cause_set_vec, type = "status")
    }

    # Loop over every combination of cc and intv
    for (i in 1:nrow(intv)) {
      ## Transform to character vector
      intv_vec <- colnames(intv)[intv[i,] %>% t() %>% magrittr::extract(,1)]
      ## Save character vector for output
      out_intv[[rownames(intv)[i]]] <- intv_vec
      ## Loop over cause_set
      for (j in 1:nrow(cause_set)) {
        ### Check intervention
        intervene_temp <- scc %>% check_causes_x_intv(cause_set = cause_set %>% dplyr::slice(j), intv = intv_vec, prc = prc, split = split,
                                                      outc_list = outc_list)
        ### Save output
        out_status[rownames(cause_set)[j],rownames(intv)[i]] <- intervene_temp$intv_status
        out_order[[rownames(intv)[i]]][[rownames(cause_set)[j]]] <- intervene_temp$intv_order
      }
    }

    # Minimize
    mini <- minimize_intv(intv = intv, out_status = out_status)

    # Combine to out_table
    out_table <- list(cause_set = out_cause_set, intv = out_intv, status = out_status, minimal = mini, order = out_order)
    #=============================================================================
    # Check output
    rlang::try_fetch({
      checkmate::assert_list(out_table, types = c("list", "data.frame"))
      checkmate::assert_set_equal(names(out_table), c("cause_set", "intv", "status", "minimal", "order"))
      checkmate::assert_list(out_table$cause_set, types = "character")
      checkmate::assert_list(out_table$intv, types = "character")
      checkmate::assert_data_frame(out_table$status, any.missing = F, null.ok = F)
      checkmate::assert_data_frame(out_table$minimal, any.missing = F, null.ok = F)
      checkmate::assert_list(out_table$order, types = "list")
      checkmate::assert_list(out_table$order[[1]], types = c("logical","data.frame"))
    }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output")
    })
    #=============================================================================
    # Return
    if (output == "table") {
      return(out_table)
    }
    if (output == "nice") {
      nice_output_intervene(out_table, scc, split)
    }
  }
}

#' Get sets of interventions for `intervene()`
#'
#' Used in `intervene()`.
#'
#' @param scc An object of class `epicmodel_scc`.
#' @param intervention The argument `intervention` from `intervene()`, which is a character vector with at least one element or NULL (default in
#' `intervene()` but not in `get_intv()`). Allowed elements are "all" and step IDs of interventions. If "all", no other elements can be specified.
#' If "all", returns all possible combinations of interventions (apart from absence of all interventions). If step IDs of interventions are
#' specified, returns all possible combinations of the specified interventions. If NULL, prints a list of all available interventions in the model
#' into the console.
#' @param split A list of sets of steps from `scc` created by `process_steplist()` and `split_prc()`.
#'
#' @returns If the SCC model has no interventions, returns an information alert to inform the user about it. If `intervention` is NULL, prints a
#' list of available interventions into the console. Otherwise, returns a data.frame with columns equal to the number of interventions and named
#' with intervention IDs, as well as one row for every set of interventions that will be investigated. Cell values are either TRUE or FALSE
#' indicating if the corresponding intervention is part of a set or not.
#'
#' @noRd
get_intv <- function(scc, intervention, split) {
  # Check input
  if (inherits(scc, "epicmodel_scc") %>% magrittr::not()) {
    cli::cli_abort(c("Input validation error: {.var scc}",
                     "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"), class = "input_scc")
  }

  rlang::try_fetch({
      checkmate::assert_character(intervention, any.missing = F, null.ok = T, min.len = 1, min.chars = 1)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var intervention}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_intervention")
  })

  rlang::try_fetch({
      checkmate::assert_list(split, any.missing = F, null.ok = F, len = 5, names = "unique")
      checkmate::assert_subset(names(split), c("non_start_steps","causes","interventions","ifnot_steps","end_steps"), empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var split}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_split")
  })
  #=============================================================================
  # Check if there are interventions
  n_intv <- nrow(split$interventions)
  if (n_intv == 0) {
    cli::cli_alert_info("No interventions have been specified in the model!")
    return(invisible(NULL))
  }

  # With no interventions specified
  if (is.null(intervention)) {
    ## Get list of interventions and combine ID and description
    list_of_steps <- split$interventions %>% dplyr::left_join(scc$steplist$step[,c("id_step","desc_step")], by = "id_step") %>%
      dplyr::select(dplyr::all_of(c("id_step","desc_step")))
    list_of_steps$merge <- paste0(list_of_steps$id_step,": ",list_of_steps$desc_step %>% stringr::str_replace("Start: ",""))
    ## Return list of interventions
    return({
      cli::cli_h2("Interventions")
      cli::cli_ul(list_of_steps$merge)
    })
  } else {
    # With one intervention sepcified
    if (length(intervention) == 1) {
      ## Check if all interventions were selected
      if (intervention == "all") {
        ### Get all combinations of interventions
        grid <- vector(mode = "list", length = n_intv)
        names(grid) <- split$interventions$id_step
        #### Include TRUE and FALSE as options for every intervention
        for (i in 1:length(grid)) {
          grid[[i]] <- c(TRUE,FALSE)
        }
        #### Expand the grid to a data.frame that contains all combinations
        grid %<>% expand.grid()
        #### Exclude the combination with all interventions as FALSE (i.e., no intervention is present)
        grid %<>% dplyr::filter(rowSums(.) > 0)
        #### Name rows
        rownames(grid) <- paste0("intv",c(1:nrow(grid)))
        ## grid is output
        intv <- grid
      } else {
        ## Check if specified interventions exists
        if (intervention %>% magrittr::is_in(split$interventions$id_step) %>% all_true() %>% magrittr::not()) {
          cli::cli_abort(c("All elements of {.var intervention} must be IDs of valid intervention steps!",
                           "i" = "Run {.fn intervene} without {.var intervention} argument to list all available intervention steps."),
                         class = "invalid_interventions")
        } else {
          intv <- matrix(rep(FALSE, n_intv), nrow = 1, ncol = n_intv) %>% as.data.frame() %>%
            magrittr::set_colnames(split$interventions$id_step) %>% magrittr::set_rownames("intv1")
          intv[1,intervention] <- TRUE
        }
      }
    } else {
      # With multiple intervetions specified
      ## Check if "all" has been specified as one element of the vector
      if (intervention %>% magrittr::is_in("all") %>% all_false() %>% magrittr::not()) {
        cli::cli_abort("In {.var intervention}, option {.field 'all'} can only be specified on its own without any other elements!",
                       class = "invalid_interventions_all")
      }
      ## Check if selected interventions exist
      if (intervention %>% magrittr::is_in(split$interventions$id_step) %>% all_true() %>% magrittr::not()) {
        invalid_intv <- intervention[intervention %>% magrittr::is_in(split$interventions$id_step) %>% magrittr::not()] %>%
          stringr::str_c(collapse = ", ")
        cli::cli_abort(c("All elements of {.var intervention} must be IDs of valid intervention steps!",
                         "i" = "The following interventions are invalid: {invalid_intv}",
                         "i" = "Run {.fn intervene} without {.var intervention} argument to list all available intervention steps."),
                       class = "invalid_interventions")
      } else {
        ### Get all combinations of selected interventions
        grid <- vector(mode = "list", length = n_intv)
        names(grid) <- split$interventions$id_step
        #### Include TRUE and FALSE as options for selected interventions and FALSE only for non-selected interventions
        for (i in 1:length(grid)) {
          if (names(grid)[i] %in% intervention) {
            grid[[i]] <- c(TRUE,FALSE)
          } else {
            grid[[i]] <- c(FALSE)
          }
        }
        #### Expand the grid to a data.frame that contains all combinations
        grid %<>% expand.grid()
        #### Exclude the combination with all interventions as FALSE (i.e., no interventions is present)
        grid %<>% dplyr::filter(rowSums(.) > 0)
        #### Name rows
        rownames(grid) <- paste0("intv",c(1:nrow(grid)))
        ## grid is output
        intv <- grid
      }
    }
  }
  #=============================================================================
  # Check output
  rlang::try_fetch({
      checkmate::assert_data_frame(intv, any.missing = F, null.ok = F, types = "logical", min.rows = 1, min.cols = 1, row.names = "unique",
                                   col.names = "unique")
      checkmate::assert_character(rownames(intv), pattern = "^intv[[:digit:]]+$")
      checkmate::assert_set_equal(colnames(intv), split$interventions$id_step)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output")
  })
  #=============================================================================
  return(intv)
}

#' Get sets of component causes for `intervene()`
#'
#' Used in `intervene()`.
#'
#' @param scc An object of class `epicmodel_scc`.
#' @param causes The argument `causes` from `intervene()`, which is a character vector with at least one element or NULL (default in
#' `intervene()` but not in `get_causes()`). Allowed elements are "all" and step IDs of causes If "all", no other elements can be specified. If
#' "all", returns element `sc_cc` from `scc`, i.e., all minimally sufficient sets of component causes. If step IDs of causes are specified, unlike
#' `get_intv()`, does not return all possible combinations of the specified causes, but only a single set with the specified causes present and all
#' other causes absent. If NULL, prints a list of all available causes in the model into the console.
#' @param split A list of sets of steps from `scc` created by `process_steplist()` and `split_prc()`.
#'
#' @returns If `causes` is NULL, prints a list of available causes into the console. Otherwise, returns a data.frame with columns equal to the
#' number of causes and named with cause IDs, as well as one row for every set of causes that will be investigated, i.e., a single row or one row
#' for every minimally sufficient set. Cell values are either TRUE or FALSE indicating if the corresponding cause is part of a set or not. If the
#' specified set of causes is not sufficient, returns an information alert to inform the user about it.
#'
#' @noRd
get_causes <- function(scc, causes, split) {
  # Check input
  if (inherits(scc, "epicmodel_scc") %>% magrittr::not()) {
    cli::cli_abort(c("Input validation error: {.var scc}",
                     "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"), class = "input_scc")
  }

  rlang::try_fetch({
      checkmate::assert_character(causes, any.missing = F, null.ok = T, min.len = 1, min.chars = 1)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var causes}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_causes")
  })

  rlang::try_fetch({
      checkmate::assert_list(split, any.missing = F, null.ok = F, len = 5, names = "unique")
      checkmate::assert_subset(names(split), c("non_start_steps","causes","interventions","ifnot_steps","end_steps"), empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var split}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
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
    # With one cause sepcified
    if (length(causes) == 1) {
      ## Check if all causes were selected
      if (causes == "all") {
        cause_set <- scc$sc_cc
      } else {
        ## Check if specified cause exists
        if (causes %>% magrittr::is_in(split$causes$id_step) %>% all_true() %>% magrittr::not()) {
          cli::cli_abort(c("All elements of {.var causes} must be IDs of valid component causes!",
                           "i" = "Run {.fn intervene} without {.var causes} argument to list all available component causes."),
                         class = "invalid_causes")
        } else {
          ## Check if causes are sufficient
          if (scc %>% are_sufficient(causes, type = "binary") %>% magrittr::not()) {
            cli::cli_alert_info("The specified set of component causes is not sufficient!")
            return(invisible(NULL))
          }
          ## Get cause_set
          cause_set <- split$causes %>% get_cause_combinations(scc$steplist)
          select <- (cause_set %>% dplyr::select(dplyr::all_of(causes)) %>% rowSums() %>% magrittr::equals(length(causes)) &
                       cause_set %>% dplyr::select(-dplyr::all_of(causes)) %>% rowSums() %>% magrittr::equals(0))
          cause_set <- cause_set %>% dplyr::filter(rownames(cause_set) %in% names(select)[select])
        }
      }
    } else {
      # With multiple causes specified
      ## Check if "all" has been specified as one element of the vector
      if (causes %>% magrittr::is_in("all") %>% all_false() %>% magrittr::not()) {
        cli::cli_abort("In {.var causes}, option {.field 'all'} can only be specified on its own without any other elements!",
                       class = "invalid_causes_all")
      }
      ## Check if selected causes exist
      if (causes %>% magrittr::is_in(split$causes$id_step) %>% all_true() %>% magrittr::not()) {
        invalid_causes <- causes[causes %>% magrittr::is_in(split$causes$id_step) %>% magrittr::not()] %>%
          stringr::str_c(collapse = ", ")
        cli::cli_abort(c("All elements of {.var causes} must be IDs of valid component causes!",
                         "i" = "The following component causes are invalid: {invalid_causes}",
                         "i" = "Run {.fn intervene} without {.var causes} argument to list all available component causes."),
                       class = "invalid_causes")
      } else {
        ## Check if causes are sufficient
        if (scc %>% are_sufficient(causes, type = "binary") %>% magrittr::not()) {
          return(cli::cli_alert_info("The specified set of component causes is not sufficient!"))
        }
        ## Get cause_set
        cause_set <- split$causes %>% get_cause_combinations(scc$steplist)
        select <- (cause_set %>% dplyr::select(dplyr::all_of(causes)) %>% rowSums() %>% magrittr::equals(length(causes)) &
                     cause_set %>% dplyr::select(-dplyr::all_of(causes)) %>% rowSums() %>% magrittr::equals(0))
        cause_set <- cause_set %>% dplyr::filter(rownames(cause_set) %in% names(select)[select])
      }
    }
  }
  #=============================================================================
  # Check output
  rlang::try_fetch({
      checkmate::assert_data_frame(cause_set, any.missing = F, null.ok = F, types = "logical", min.rows = 1, min.cols = 1, row.names = "unique",
                                   col.names = "unique")
      checkmate::assert_character(rownames(cause_set), pattern = "^cc[[:digit:]]+$")
      checkmate::assert_set_equal(colnames(cause_set), split$causes$id_step)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output")
  })
  #=============================================================================
  return(cause_set)
}

#' Causes prevented by interventions
#'
#' Get component causes, for which IFNOT conditions are fulfilled by a certain set of interventions. Used in `check_causes_x_intv()`.
#'
#' @param causes A tibble, which is a subset of processed data created by `process_steplist()`. The subsetting has been done with `split_prc()`.
#' The element used here is subset `causes`.
#' @param intv A character vector containing the list of intervention variables specified in `intervene()`.
#'
#' @returns A character vector containing IDs of causes, for which the set of interventions `intv` fulfills the IFNOT condition. If no cause has an
#' IFNOT condition or if no IFNOT condition is fulfilled, returns an empty character vector of length 0.
#'
#' @noRd
get_prevented_causes <- function(causes, intv) {
  # Check input
  rlang::try_fetch({
      checkmate::assert_tibble(causes, null.ok = F, ncols = 12, min.rows = 1, col.names = "unique")
      checkmate::assert_subset(colnames(causes), c("id_step","then_step","subject_step","does_step","object_step","where_step","if_step","if_list",
                                                "ifnot_step","ifnot_list","end_step","module_step"), empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var causes}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_causes")
  })

  rlang::try_fetch({
      checkmate::assert_character(intv, any.missing = F, null.ok = F, min.chars = 1, min.len = 1, unique = TRUE)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var intv}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_intv")
  })
  #=============================================================================
  # Get causes with IFNOT condition
  relevant_causes <- causes %>% dplyr::filter(!is.na(.data$ifnot_step))

  # No cause has IFNOT condition
  if (nrow(relevant_causes) == 0) {
    return(character(0))
  } else {
  # There are causes with IFNOT conditions
    ## Check if intervention set intv fulfills IFNOT conditions
    ### Create empty container
    fulfilled_causes <- rep(NA, nrow(relevant_causes)) %>% magrittr::set_names(relevant_causes$id_step)
    for (i in 1:length(fulfilled_causes)) {
      ### Actually check if IFNOT is fulfilled
      fulfilled_causes[i] <- is_fulfilled(if_list = relevant_causes$ifnot_list[relevant_causes$id_step == names(fulfilled_causes)[i]] %>%
                                                      magrittr::extract2(1),
                                          current_list_then = intv %>% sep_step() %>% magrittr::extract2("then"))
    }
    ## No IFNOT condition is fulfilled
    if (fulfilled_causes %>% all_false()) {
      return(character(0))
    } else {
    ## Some IFNOT conditions are fulfilled
      out <- names(fulfilled_causes)[fulfilled_causes]
      #=============================================================================
      # Check output
      rlang::try_fetch({
          checkmate::assert_character(out, any.missing = F, null.ok = F, min.chars = 1, min.len = 1, unique = TRUE)
          checkmate::assert_subset(out, causes$id_step, empty.ok = F)
        }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                                 "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                                 parent = cnd, class = "output")
      })
      #=============================================================================
      return(out)
    }
  }
}

#' Non-starting steps prevented by interventions
#'
#' Get non-starting steps, for which IFNOT conditions are fulfilled by a certain set of interventions. Used in `check_causes_x_intv()`.
#'
#' @param non_start_steps A tibble, which is a subset of processed data created by `process_steplist()`. The subsetting has been done with
#' `split_prc()`.The element used here is subset `non_start_steps`.
#' @param intv A character vector containing the list of intervention variables specified in `intervene()`.
#'
#' @returns A character vector containing IDs of non-starting steps, for which the set of interventions `intv` fulfills the IFNOT condition.
#' If no step has an IFNOT condition or if no IFNOT condition is fulfilled, returns an empty character vector of length 0.
#'
#' @noRd
get_prevented_non_start_steps <- function(non_start_steps, intv) {
  # Check input
  rlang::try_fetch({
      checkmate::assert_tibble(non_start_steps, null.ok = F, ncols = 12, col.names = "unique")
      checkmate::assert_subset(colnames(non_start_steps), c("id_step","then_step","subject_step","does_step","object_step","where_step","if_step",
                                                            "if_list","ifnot_step","ifnot_list","end_step","module_step"), empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var non_start_steps}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_non_start_steps")
  })

  rlang::try_fetch({
      checkmate::assert_character(intv, any.missing = F, null.ok = F, min.chars = 1, min.len = 1, unique = TRUE)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var intv}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_intv")
  })
  #=============================================================================
  # Get non-start steps with IFNOT condition
  relevant_steps <- non_start_steps %>% dplyr::filter(!is.na(.data$ifnot_step))

  # No step has IFNOT condition or non_start_steps is empty
  if (nrow(relevant_steps) == 0) {
    return(character(0))
  } else {
    # There are steps with IFNOT conditions
    ## Check if intervention set intv fulfills IFNOT conditions
    ### Create empty container
    fulfilled_steps <- rep(NA, nrow(relevant_steps)) %>% magrittr::set_names(relevant_steps$id_step)
    for (i in 1:length(fulfilled_steps)) {
      ### Actually check if IFNOT is fulfilled
      fulfilled_steps[i] <- is_fulfilled(if_list = relevant_steps$ifnot_list[relevant_steps$id_step == names(fulfilled_steps)[i]] %>%
                                            magrittr::extract2(1),
                                          current_list_then = intv %>% sep_step() %>% magrittr::extract2("then"))
    }
    ## No IFNOT condition is fulfilled
    if (fulfilled_steps %>% all_false()) {
      return(character(0))
    } else {
      ## Some IFNOT conditions are fulfilled
      out <- names(fulfilled_steps)[fulfilled_steps]
      #=============================================================================
      # Check output
      rlang::try_fetch({
          checkmate::assert_character(out, any.missing = F, null.ok = F, min.chars = 1, min.len = 1, unique = TRUE)
          checkmate::assert_subset(out, non_start_steps$id_step, empty.ok = F)
        }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                                 "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                                 parent = cnd, class = "output")
      })
      #=============================================================================
      return(out)
    }
  }
}

#' Get sufficiency status with interventions
#'
#' Checks sufficiency for a single set of component causes and a single set of interventions. Is called repeatedly in `intervene()` to check all
#' specified combinations of causes and interventions.
#'
#' @param scc An object of class `epicmodel_scc`.
#' @param cause_set A data.frame with columns equal to the number of causes and named with cause IDs. Contains a single row, which corresponds to the
#' investigated set of causes. Cell values are either TRUE or FALSE indicating if the corresponding cause is part of a set or not. It's the output of
#' `get_causes()` subsetted to a single row.
#' @param intv A character vector containing the step IDs of the interventions in the investigated intervention set.
#' @param prc The processed steplist created from `scc$steplist` by `process_steplist()`.
#' @param split The processed steplist split into relevant parts created from `prc` by `split_prc()`.
#' @param outc_list The output of function `transform_outc()`.
#'
#' @returns A list of length 2 with elements `intv_status`, which is one of "always", "depends", or "never" ("depends (potential order
#' implausibilities)" becomes "depends"), and `intv_order`, which is NA, if `intv_status` is "always" or "never", or a data.frame with two columns
#' (`order` and `suff`), with `order` containing all relevant sequences of events as character and `suff` containing TRUE or FALSE indicating
#' sufficiency for the corresponding sequence, if `intv_status` is "depends". This data.frame is element `order` from the output of `check_ifnot()`.
#'
#' @noRd
check_causes_x_intv <- function(scc, cause_set, intv, prc, split, outc_list) {
  # Check inputs
  if (inherits(scc, "epicmodel_scc") %>% magrittr::not()) {
    cli::cli_abort("Input validation error: {.var scc}", class = "input_scc")
  }

  rlang::try_fetch({
      checkmate::assert_data_frame(cause_set, any.missing = F, null.ok = F, types = "logical", nrows = 1, min.cols = 1, row.names = "unique",
                                   col.names = "unique")
      checkmate::assert_character(rownames(cause_set), pattern = "^cc[[:digit:]]+$")
      checkmate::assert_set_equal(colnames(cause_set), split$causes$id_step)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var cause_set}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_cause_set")
  })

  rlang::try_fetch({
      checkmate::assert_character(intv, any.missing = F, null.ok = F, min.len = 1, min.chars = 1)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var intv}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_intv")
  })

  rlang::try_fetch({
      checkmate::assert_tibble(prc, null.ok = F, ncols = 12, min.rows = 1, col.names = "unique")
      checkmate::assert_subset(colnames(prc), c("id_step","then_step","subject_step","does_step","object_step","where_step","if_step","if_list",
                                                "ifnot_step","ifnot_list","end_step","module_step"), empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var prc}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_prc")
  })

  rlang::try_fetch({
      checkmate::assert_list(split, any.missing = F, null.ok = F, len = 5, names = "unique")
      checkmate::assert_subset(names(split), c("non_start_steps","causes","interventions","ifnot_steps","end_steps"), empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var split}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_split")
  })

  end_then <- prc %>% dplyr::filter(.data$end_step == "1") %>% magrittr::extract2("then_step")
  rlang::try_fetch({
      checkmate::assert_data_frame(outc_list, types = "character", any.missing = F, null.ok = F, ncols = 2, min.rows = 1, col.names = "unique")
      checkmate::assert_subset(colnames(outc_list), c("sce","id"), empty.ok = F)
      checkmate::assert_integerish(outc_list$sce %>% as.numeric(), lower = 1, any.missing = F, null.ok = F)
      checkmate::assert_subset(outc_list$id, end_then, empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_outc_list")
  })
  #=============================================================================
  # Get prevented causes
  prev_causes <- get_prevented_causes(split$causes, intv)
  ## Prepare causes
  if (length(prev_causes) > 0) {
    cause_set[,prev_causes] <- FALSE
  }

  # Get prevented non-starting steps
  prev_non_start_steps <- get_prevented_non_start_steps(split$non_start_steps, intv)
  ## Prepare non_start_steps
  non_start_steps_intv <- split$non_start_steps
  if (length(prev_non_start_steps) > 0) {
    non_start_steps_intv %<>% dplyr::filter(!(.data$id_step %in% prev_non_start_steps))
  }

  # Check sufficiency
  ## Check if all component causes are FALSE
  if (cause_set %>% as.logical() %>% all_false()) {
    sufficient <- FALSE
    final_steps <- character(0)
  } else {
    ## Actually check sufficiency
    suff_results <- is_sufficient(cc = cause_set, row = 1, non_start_steps = non_start_steps_intv, outc_list = outc_list)
    ## Process is_sufficient() output
    sufficient <- suff_results[["is_suff"]]
    final_steps <- list(suff_results[["final_list"]]) %>% magrittr::set_names(rownames(cause_set))
  }

  # Check IFNOT conditions
  ## Skip if the interventions already prevented all sufficient causes
  if (sufficient) {
    ## Skip if there are no steps with IFNOT conditions
    if (length(split$ifnot_steps) > 0) {
      ## Get list of sufficient causes that need to be re-evaluated for IFNOT conditions
      re_sc_intv <- get_sc_to_check_for_ifnot(sc = cause_set, sc_final_steps = final_steps, ifnot_steps = split$ifnot_steps)
      if (nrow(re_sc_intv) == 0) {
        intv_status <- "always"
        intv_order <- NA
      } else {
        ## Check IFNOT conditions
        check_ifnot_temp <- check_ifnot(re_sc = re_sc_intv, row = 1, sc_final_steps = final_steps,
                                          prc = prc, prc_split = split, outc_list = outc_list)
        ## Process check_ifnot() output
        intv_status <- check_ifnot_temp$sc_status
        intv_order <- check_ifnot_temp$order
      }
    } else {
      intv_status <- "always"
      intv_order <- NA
    }
  } else {
    intv_status <- "never"
    intv_order <- NA
  }

  # Return result
  if (intv_status == "depends (potential order implausibilities)") {
    intv_status <- "depends"
  }
  out <- list(intv_status = intv_status, intv_order = intv_order)
  #=============================================================================
  # Check output
  rlang::try_fetch({
      checkmate::assert_list(out, types = c("data.frame","logical","character"), null.ok = F, len = 2, names = "unique")
      checkmate::assert_set_equal(names(out), c("intv_status","intv_order"), ordered = T)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var out}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output_out")
  })

  rlang::try_fetch({
    checkmate::assert_character(out$intv_status, any.missing = F, null.ok = F, len = 1, min.chars = 1)
    checkmate::assert_choice(out$intv_status, c("always", "never", "depends"))
    }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var out$intv_status}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output_out$intv_status")
  })

  if (is.na(out$intv_order) %>% all_true()) {
    rlang::try_fetch({
      checkmate::assert_scalar_na(out$intv_order, null.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var out$intv_order}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output_out$intv_order")
    })
  } else {
    rlang::try_fetch({
      checkmate::assert_data_frame(out$intv_order, types = c("character","logical"), any.missing = F, null.ok = F, ncols = 2,
                                   col.names = "unique")
      checkmate::assert_set_equal(colnames(out$intv_order), c("order", "suff"), ordered = T)
      checkmate::assert_logical(out$intv_order$suff, any.missing = F, null.ok = F)
      checkmate::assert_character(out$intv_order$order, any.missing = F, null.ok = F, min.chars = 1, unique = T)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var out$intv_order}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output_out$intv_order")
    })
  }
  #=============================================================================
  return(out)
}

#' Select minimal sets of interventions
#'
#' Used in `intervene()`. Uses same algorithm as described in `minimize_sc()`.
#'
#' @details The following algorithm is used to select minimal intervention sets:
#'  * Get candidates for minimal interventions sets: Due to the three different sufficiency statuses, i.e., "always", "depends", "never", a
#'    pre-selection of candidate sets that might be minimal needs to be done. The candidates depend on the sufficiency status without intervention
#'    (intv0) and if there are any intervention sets with status "never", i.e., complete prevention of outcome. If sufficiency status of intv0 is
#'    "always" and intervention sets with "never" are available, all interventions with status "never" are candidates but intervention sets with
#'    status "depends" are not. If no intervention set has status "never", all sets with status "depends" are candidates. If intv0 has sufficiency
#'    status "depends", only intervention sets with status "never" can be candidates.
#'  * Among the candidate sets, the criteria described in `minimize_sc()` are applied to identify minimal sets. (The implementation is slightly
#'    different.)
#'
#' @param intv The output of `get_intv()`: a data.frame with columns equal to the number of interventions and named with intervention IDs, as well
#' as one row for every set of interventions that will be investigated. Cell values are either TRUE or FALSE indicating if the corresponding
#' intervention is part of a set or not.
#' @param out_status Created in `intervene()` before `minimize_intv` is called. A data.frame with one row per set of component causes investigated
#' in `intervene()` and one column per set of interventions investigated by `intervene()`. In addition, there is one column representing no
#' interventions (intv0). Columns have intervention IDs, e.g., intv1, intv2, etc. and rows have IDs of sufficient causes or sets of component cause
#' set, e.g., cc1, cc2, etc. Cell values are "always", "depends", or "never", i.e., describe the sufficiency status. For intv0, sufficiency status
#' is derived by `are_sufficient(type = "status")`. For all other columns, sufficiency status is derived by `check_causes_x_intv()` and its output
#' element `intv_status`.
#'
#' @returns A data.frame similar in format to `out_status` but without column `intv0`. Cell values are TRUE or FALSE indicating, if the corresponding
#' intervention is minimal.
#'
#' @noRd
minimize_intv <- function(intv, out_status) {
  # Check input
  rlang::try_fetch({
      checkmate::assert_data_frame(intv, any.missing = F, null.ok = F, types = "logical", min.rows = 1, min.cols = 1, row.names = "unique",
                                   col.names = "unique")
      checkmate::assert_character(rownames(intv), pattern = "^intv[[:digit:]]+$")
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var intv}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_intv")
  })

  rlang::try_fetch({
      checkmate::assert_data_frame(out_status, any.missing = F, null.ok = F, types = "character", min.rows = 1, min.cols = 1, row.names = "unique",
                                   col.names = "unique")
      checkmate::assert_character(colnames(out_status), pattern = "^intv[[:digit:]]+$")
      checkmate::assert_character(rownames(out_status), pattern = "^cc[[:digit:]]+$")
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var out_status}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_out_status")
  })
  #=============================================================================
  # Create container
  mini <- matrix(rep(FALSE, nrow(out_status) * nrow(intv)), nrow = nrow(out_status), ncol = nrow(intv)) %>% as.data.frame() %>%
    magrittr::set_rownames(rownames(out_status)) %>% magrittr::set_colnames(rownames(intv))

  # Loop over every cause_set
  for (i in 1:nrow(mini)) {
    ## Get candidates for minimal interventions sets
    intv0_status <- out_status[i,"intv0"]
    # LEGACY: intv_all_status <- out_status[i, c(2:ncol(out_status))]
    intv_all_status <- out_status %>% dplyr::select(2:dplyr::last_col()) %>% dplyr::slice(i)
    never_exists <- "never" %>% magrittr::equals(intv_all_status) %>% all_false() %>% magrittr::not()
    never_which <- colnames(intv_all_status)["never" %>% magrittr::equals(intv_all_status)]
    depends_which <- colnames(intv_all_status)["depends" %>% magrittr::equals(intv_all_status)]

    if (intv0_status == "always" & never_exists) {
      cand <- never_which
    }
    if (intv0_status == "always" & !never_exists) {
      cand <- depends_which
    }
    if (intv0_status == "depends") {
      cand <- never_which
    }
    if (intv0_status == "never") {
      cand <- character(0)
    }

    #intv_temp <- intv[cand,]
    intv_temp <- intv %>% dplyr::filter(rownames(intv) %in% cand)
    if (nrow(intv_temp) > 0) {
      # Check minimality
      n_rows <- nrow(intv_temp)
      minimal <- rep(TRUE, n_rows) %>% magrittr::set_names(rownames(intv_temp))

      for (j in 1:n_rows) {
        ## Which component causes are FALSE for the j-th set
        left_out_start <- colnames(intv_temp)[which(intv_temp[j, ] == F)]
        ## Loop over all sufficient sets of component causes (to compare with the j-th set)
        for (k in 1:n_rows) {
          ### Check if the component causes that are missing in the j-th set, are also missing in the k-th set
          if (length(left_out_start) > 0) {
            also_false <- intv_temp[k,left_out_start] %>% as.logical() %>% all_false()
          } else {
            also_false <- T
          }
          ### Check if the k-th set contains less component causes than the j-th set
          less_starts <- intv_temp[k,] %>% sum() %>% magrittr::is_less_than(intv_temp[j, ] %>% sum())
          ### If both is TRUE for any k-th set, the j-th set is not minimal, i.e., at least on of the k-th sets is smaller
          if (also_false & less_starts) {
            minimal[j] <- FALSE
          }
        }
      }

    mini[i,names(minimal)[minimal]] <- TRUE
    }
  }
  #=============================================================================
  # Check output
  rlang::try_fetch({
      checkmate::assert_data_frame(mini, any.missing = F, null.ok = F, types = "logical", min.rows = 1, min.cols = 1, row.names = "unique",
                                   col.names = "unique")
      checkmate::assert_character(colnames(mini), pattern = "^intv[[:digit:]]+$")
      checkmate::assert_character(rownames(mini), pattern = "^cc[[:digit:]]+$")
    }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output")
  })
  #=============================================================================
  return(mini)
}

#' Create nice output for `intervene()`
#'
#' Used in `intervene()` if `output = "nice"`.
#'
#' @param out_table The output of `intervene()`, if `output == "table"`.
#' @param scc Argument `scc` from the corresponding call of `intervene()`.
#' @param split A list of sets of steps from `scc` created by `process_steplist()` and `split_prc()`.
#'
#' @returns Nothing, but prints a nicely formatted version of `out_table` into the console.
#'
#' @noRd
nice_output_intervene <- function(out_table, scc, split) {
  # Check inputs
  if (inherits(scc, "epicmodel_scc") %>% magrittr::not()) {
    cli::cli_abort("Input validation error: {.var scc}",
                   class = "input_scc")
  }

  rlang::try_fetch({
      checkmate::assert_list(split, any.missing = F, null.ok = F, len = 5, names = "unique")
      checkmate::assert_subset(names(split), c("non_start_steps","causes","interventions","ifnot_steps","end_steps"), empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var split}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_split")
  })
  #=============================================================================
  # Merge THEN description to processed steplist
  split$interventions %<>% dplyr::left_join(scc$steplist$then, by = c("then_step" = "id_then"))
  split$causes %<>% dplyr::left_join(scc$steplist$then, by = c("then_step" = "id_then"))


  # Start with headline
  cli::cli_h1("Intervention")

  # Loop over cause sets
  for (i in 1:nrow(out_table$status)) {
    causes_temp <- out_table$cause_set[[rownames(out_table$status)[i]]]
    minimal_temp <- colnames(out_table$minimal)[out_table$minimal[i,] %>% t() %>% magrittr::extract(,1)]
    if (length(minimal_temp) > 0) {
      never_exists_temp <- "never" %>% magrittr::equals(out_table$status[i,minimal_temp]) %>% all_false() %>% magrittr::not()
      intv_temp <- vector(mode = "list", length = length(minimal_temp)) %>% magrittr::set_names(minimal_temp)
      for (j in 1:length(intv_temp)) {
        intv_temp[[j]] <- out_table$intv[[names(intv_temp)[j]]]
      }
    }

    cli::cli_h2(paste0("Cause Set ",i))
    cli::cli_ul(split$causes$desc_then[split$causes$id_step %in% causes_temp])
    cat("\n")
    cli::cli_text("Status without intervention")
    if (out_table$status$intv0[i] == "always") {
      cli::cli_alert_success("Always sufficient")
      if (length(minimal_temp) == 0) {
        cat("\n")
        cli::cli_text("Status with intervention")
        cli::cli_alert_danger("No considered intervention is able to prevent the outcome")
      } else {
        if (never_exists_temp) {
          cat("\n")
          cli::cli_text("Status with intervention")
          cli::cli_alert_success("Complete prevention by the following minimal intervention sets")
          for (j in 1:length(minimal_temp)) {
            cli::cli_h3(paste0("Intervention Set ",j))
            cli::cli_ul(split$interventions$desc_then[split$interventions$id_step %in% intv_temp[[minimal_temp[j]]]])
          }
        } else {
          cat("\n")
          cli::cli_text("Status with intervention")
          cli::cli_alert_warning("No complete prevention. Prevention of the following intervention sets depend on order of occurrence")
          for (j in 1:length(minimal_temp)) {
            cli::cli_h3(paste0("Intervention Set ",j))
            cli::cli_ul(split$interventions$desc_then[split$interventions$id_step %in% intv_temp[[minimal_temp[j]]]])
            cat("\n")
            cli::cli_text("Prevention (no sufficiency) for the following orders of occurrence")
            order_tab <- out_table$order[[minimal_temp[j]]][[rownames(out_table$status)[i]]]
            if (order_tab %>% is.na() %>% all_true() %>% magrittr::not()) {
              ## Choose sufficient == FALSE because those are the orders of occurrence for which the outcome is prevented by the intervention
              cli::cli_ul(sufficient_order(order_tab, scc$steplist, sufficient = FALSE))
            }
          }
        }
      }
    }
    if (out_table$status$intv0[i] == "depends") {
      cli::cli_alert_warning("Sufficiency depends on order of occurrence")
      if (length(minimal_temp) == 0) {
        cat("\n")
        cli::cli_text("Status with intervention")
        cli::cli_alert_danger("No considered intervention is able to prevent the outcome")
      } else {
        cat("\n")
        cli::cli_text("Status with intervention")
        cli::cli_alert_success("Complete prevention by the following minimal intervention sets")
        for (j in 1:length(minimal_temp)) {
          cli::cli_h3(paste0("Intervention Set ",j))
          cli::cli_ul(split$interventions$desc_then[split$interventions$id_step %in% intv_temp[[minimal_temp[j]]]])
        }
      }
    }
    if (out_table$status$intv0[i] == "never") {
      cli::cli_alert_danger("Never sufficient -> No intervention necessary")
    }
  }
}
