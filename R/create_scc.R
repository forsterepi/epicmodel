#' Creating SCC models
#'
#' Creates a sufficient-components cause (SCC) model from a steplist, which is a list of IF/THEN statements describing the causal mechanism behind
#' an outcome of interest. The steplist needs to meet cretain structural requirements. Therefore, for steplist creation, use the Steplist Creator
#' `shiny` app launched by [launch_steplist_creator()].
#'
#' @details The following algorithm is used to create a sufficient-component cause (SCC) model from a steplist.
#'   * Check inputs: The steplist needs to be checked by [check_steplist()] before input
#'   * Are modules used: Evaluate if the steplist contains modules
#'   * Process steplist: Process steplist and outcome definition so that they can be used by the procedure
#'   * Get all combinations of component causes in the steplist: Component causes are steps, which themselves have no IF condition but appear
#'     in IF conditions of other steps (and maybe additionally in IFNOT conditions). Interventions are not considered to be component causes.
#'     Interventions are as well steps without IF condition, but they only appear in IFNOT conditions of other steps. Invalid combinations of
#'     component causes as specified in the ICC part of the steplist are excluded, as well as every component cause being FALSE.
#'   * Check sufficiency: Sufficiency is checked for every combination of component causes. First, based on a specific set of component causes,
#'     it is derived, which steps can be caused by this set, i.e., which IF conditions are fulfilled. For this, a current set of included steps is
#'     defined, which in the beginning includes only the corresponding set of component causes. Then, it is iteratively checked, for which other
#'     steps with IF condition (i.e., excluding non-selected component causes and interventions) this IF condition is fulfilled. These steps are
#'     added to the current set of included steps and the process is repeated until for no new steps the IF condition is fulfilled. Second, this
#'     final list of steps is compared against the outcome definitions. If it is fulfilled, the set of component causes is sufficient.
#'  * Check IFNOT conditions: Please note that IFNOT conditions were ignored up to this point. Now, all sets of component causes that were found
#'    to be sufficient previously, are re-checked for IFNOT conditions. First, it is checked if there are any IFNOT conditions in the final list of
#'    steps derived above and if those are fulfilled based only on the other steps in this list. If no, checking is complete and the corresponding
#'    set of component causes is always sufficient. If yes, further checking is required. In these cases, sufficiency depends on the order in which
#'    individual steps occur. In principle, a step with both IF and IFNOT conditions fulfilled, occurs if the IF condition is fulfilled before the
#'    IFNOT condition, similar to how I do not care if a door is closed if I already went through it when it was still open. Please note that this
#'    approach extends SCC models by an additional time component. Sufficiency is therefore re-checked for all possible sequences of IF and IFNOT
#'    conditions of all steps that include IFNOT conditions that can be fulfilled by the final set of steps. It is possible to have component causes
#'    with IFNOT conditions. Since they do not have an IF condition, the THEN statement is used instead. For every sequence, it is evaluated if the
#'    IF (or THEN for component causes) occurs before or after the IFNOT. If IF/THEN occur after the corresponding IFNOT, this step is removed from
#'    the final list of steps. Sufficieny is now re-checked based on the updated list. If some orderings do not fulfill the outcome definition,
#'    the sufficiency status of the corresponding set of component causes is changed to "depends", as it depends on the sequence of events. Please
#'    note that currently, all sequences are checked even though some of them might be implausible, e.g., when two steps with IFNOT conditions are
#'    chained together. In this case, there will be a warning displayed, but the user ultimately needs to check plausibility of the sequence of
#'    events.
#'  * Minimize: Sufficient causes must be minimal by definition, i.e., every component cause must be necessary within its sufficient cause, i.e.,
#'    the absence of one component cause of a sufficient set means that the outcome does not occur anymore. Therefore, the list of sufficient (both
#'    always and depends) sets of component causes is reduced to minimal ones.
#'  * Add unknown causes: It is possible/likely that unknown causes, both component causes and sufficient causes, are not part of the model yet.
#'    Therefore, every sufficient cause gets an additional individual (i.e., a different one for each sufficient cause) unknown component cause
#'    representing additional unknown components, and one unknown sufficient cause is added to the model consisting of a single unknown component
#'    cause and representing all unknown sufficient causes. If relevant, the user can decide in functions with the SCC model as input if unknown
#'    causes ahould be included or not.
#'  * Output preparation: Combines all outputs to an object of class `epicmodel_scc` for further analysis.
#'
#' @param steplist An object of class `epicmodel_steplist_checked`.
#'
#' @returns An object of class `epicmodel_scc`. If no sufficient causes are found, no object is returned but instead a corresponding message
#' is displayed in the console.
#'
#' @references Rothman KJ (1976): Causes. American Journal of Epidemiology 104 (6): 587–592.
#'
#' @seealso
#' * \code{\link[=new_scc]{SCC models}} for information on `epicmodel_scc` objects
#' * \code{\link[=new_steplist]{Steplist}} for information on `epicmodel_steplist` objects
#'
#' @export
#'
#' @examples
#' # First, create a steplist in the shiny app
#' # Launch the app with launch_steplist_creator()
#' # Then load your steplist using readRDS()
#' # In this example we use the built-in steplist_rain
#'
#' # Check the steplist before running create_scc()
#' steplist_checked <- check_steplist(steplist_rain)
#'
#' # Use the checked steplist in create_scc()
#' scc_model <- create_scc(steplist_checked)
create_scc <- function(steplist) {
  # Check input
  if (inherits(steplist, c("epicmodel_steplist_checked","epicmodel_steplist")) %>% magrittr::not()) {
    cli::cli_abort("{.var steplist} must be a {.emph epicmodel_steplist} class object, which has been successfully checked
                   by {.code check_steplist}!",
                   class = "no_steplist")
  }

  # Steplist needs to be checked before using it here
  if (inherits(steplist, "epicmodel_steplist")) {
    cli::cli_abort("You need to successfully check your {.emph steplist} using {.code check_steplist()} before using {.code create_scc()}!",
                   class = "no_checked_steplist")
  }
  #=============================================================================
  # Get module usage
  use_modules <- are_modules_used(steplist)

  # Process steplist into the parts needed for scc creation
  prc <- steplist %>% process_steplist()
  outc_list <- steplist %>% transform_outc()
  prc_split <- split_prc(prc)

  # Get all potential combinations of component causes
  cc <- prc_split[["causes"]] %>% get_cause_combinations(steplist)

  # Check sufficiency
  ## Set up output container
  sufficient <- vector(mode = "logical", length = nrow(cc))
  final_steps <- vector(mode = "list", length = nrow(cc))
  ## Loop over all potential combinations of component causes
  cli::cli_h2("Create SCC Model")
  cli::cli_progress_bar("Check if set of component causes is sufficient", total = length(sufficient), type = "tasks")
  for (i in 1:length(sufficient)) {
    ### Actually check sufficiency
    suff_results <- is_sufficient(cc = cc, row = i, non_start_steps = prc_split[["non_start_steps"]], outc_list = outc_list)
    ### Process is_sufficient() output
    sufficient[i] <- suff_results[["is_suff"]]
    names(sufficient)[i] <- rownames(cc)[i]
    final_steps[[i]] <- suff_results[["final_list"]]
    names(final_steps)[i] <- rownames(cc)[i]
    cli::cli_progress_update()
  }
  cli::cli_alert_success(paste0(length(sufficient),"/",length(sufficient)," | Check if set of component causes is sufficient"))
  ## Extract sufficient combinations of component causes
  # LEGACY: sc <- cc[sufficient,]
  sc <- cc %>% dplyr::filter(rownames(cc) %in% names(sufficient)[sufficient])

  # Exit if no sufficient cause has been found
  if (nrow(sc) == 0) {
    cli::cli_alert_info("There are no sufficient causes!")
    return(invisible(NULL))
  }

  # Check IFNOT conditions
  ## Skip if there are no steps with IFNOT conditions
  if (length(prc_split[["ifnot_steps"]]) > 0) {
    ## Get list of steps included in the sufficient causes
    sc_final_steps <- final_steps[rownames(sc)]
    ## Get list of sufficient causes that need to be re-evaluated for IFNOT conditions
    re_sc <- get_sc_to_check_for_ifnot(sc = sc, sc_final_steps = sc_final_steps, ifnot_steps = prc_split[["ifnot_steps"]])
    if (nrow(re_sc) == 0) {
      sufficiency_status <- rep("always", nrow(sc)) %>% magrittr::set_names(rownames(sc))
    } else {
      ## Check IFNOT conditions
      ### Set up output containers
      sufficiency_status_re_sc <- rep("", nrow(re_sc)) %>% magrittr::set_names(rownames(re_sc))
      order_re_sc <- vector(mode = "list", length = nrow(re_sc)) %>% magrittr::set_names(rownames(re_sc))
      incon_re_sc <- rep(FALSE, nrow(re_sc)) %>% magrittr::set_names(rownames(re_sc))
      incon_then_re_sc <- vector(mode = "list", length = nrow(re_sc)) %>% magrittr::set_names(rownames(re_sc))
      cli::cli_progress_bar("Check if sufficiency dependends on IFNOT conditions", total = nrow(re_sc), type = "tasks")
      for (i in 1:nrow(re_sc)) {
        ### Actually check IFNOT conditions
        check_ifnot_temp <- check_ifnot(re_sc, row = i, sc_final_steps, prc, prc_split, outc_list)
        ### Process check_ifnot() output
        sufficiency_status_re_sc[i] <- check_ifnot_temp$sc_status
        order_re_sc[[i]] <- check_ifnot_temp$order
        incon_re_sc[i] <- check_ifnot_temp$incon
        incon_then_re_sc[[i]] <- check_ifnot_temp$incon_then
        cli::cli_progress_update()
      }
      cli::cli_alert_success(paste0(nrow(re_sc),"/",nrow(re_sc)," | Check if sufficiency dependends on IFNOT conditions"))
      ## Combine results with other sufficient causes not in re_sc
      rownames_sc_not_re <- rownames(sc)[rownames(sc) %>% magrittr::is_in(rownames(re_sc)) %>% magrittr::not()]
      sufficiency_status_sc <- rep("always", length(rownames_sc_not_re)) %>% magrittr::set_names(rownames_sc_not_re)
      sufficiency_status <- c(sufficiency_status_sc,sufficiency_status_re_sc) %>%
        magrittr::extract(names(.) %>% stringr::str_sub(3,-1) %>% as.integer() %>% order())
      ## Remove sc_status == "never"
      sufficiency_status %<>% .[. != "never"]
      # sc %<>% .[names(sufficiency_status),]
      sc %<>% dplyr::filter(rownames(sc) %in% names(sufficiency_status))
    }
  } else {
    sufficiency_status <- rep("always", nrow(sc)) %>% magrittr::set_names(rownames(sc))
  }

  # Check sufficient causes for minimality
  sc %<>% minimize_sc()

  # Create output
  sufficiency_status %<>% .[rownames(sc)]
  sc_final_steps <- final_steps[rownames(sc)]
  order_out <- vector(mode = "list", length = nrow(sc)) %>% magrittr::set_names(rownames(sc))
  for (i in 1:length(order_out)) {
    if (exists("order_re_sc")) {
      if (names(order_out)[i] %in% names(order_re_sc)) {
        order_out[[i]] <- order_re_sc[[names(order_out)[i]]]
      } else {
        order_out[[i]] <- NA
      }
    } else {
      order_out[[i]] <- NA
    }
  }
  incon_out <- rep(FALSE, nrow(sc)) %>% magrittr::set_names(rownames(sc))
  for (i in 1:length(incon_out)) {
    if (exists("incon_re_sc")) {
      if (names(incon_out)[i] %in% names(incon_re_sc)) {
        incon_out[i] <- incon_re_sc[names(incon_out)[i]]
      }
    }
  }
  incon_then_out <- vector(mode = "list", length = nrow(sc)) %>% magrittr::set_names(rownames(sc))
  for (i in 1:length(incon_then_out)) {
    if (exists("incon_then_re_sc")) {
      if (names(incon_then_out)[i] %in% names(incon_then_re_sc)) {
        incon_then_out[[i]] <- incon_then_re_sc[[names(incon_then_out)[i]]]
      } else {
        incon_then_out[[i]] <- NA
      }
    } else {
      incon_then_out[[i]] <- NA
    }
  }
  ## Add unknown causes
  unknown_cc <- sc %>% unknown_sc()
  unknown_status <- c(sufficiency_status, cc0 = "unknown")
  ## Combine
  out <- list(sc_cc = sc, sc_status = sufficiency_status, sc_steps = sc_final_steps, sc_order = order_out,
              sc_implausibilities = incon_out, sc_implausibilities_detail = incon_then_out, sc_use_modules = use_modules,
              unknown_cc = unknown_cc, unknown_status = unknown_status, steplist = steplist)

  # Make epicmodel_scc object
  out %<>% new_scc()
  # Validate epicmodel_scc object
  out %<>% validate_scc()
  # Return
  return(out)
}

#' Determine standardized effect size of component causes
#'
#' SCC models teach us that effect strength, e.g., a risk ratio, is no natural constant but depends on the prevalence of component causes and,
#' therefore, differs between populations. However, even without any population, this function derives effect sizes for every component cause by
#' comparing how many sets of component causes with and without a certain cause are sufficient to cause the outcome of interest.
#'
#' @details The following algorithm is used to derive effect sizes from SCC models:
#'  * The effect size is derived for one specific component cause. The following steps are repeated for all of them.
#'  * Get all potential combinations of component causes
#'  * Remove combinations that contain incompatible component causes (ICC), as specified in the steplist
#'  * Split the set of possible combinations of component causes into two parts: Sets, in which the component cause of interest is present & sets,
#'    in which the component cause of interest is absent. The numbers are recorded and returned in the output table (output = "table") as variables
#'    `num_combos_true` (cause is present) and `num_combos_false` (cause is absent). If there are no incompatible component causes (ICC), both
#'    values should be the same.
#'  * Check for all possible combinations of component causes, if they are sufficient for the outcome to occur. The number of sufficient combinations
#'    are counted separately for combinations with the component cause of interest present and combinations with the component cause of interest
#'    absent. The numbers are recorded and returned in the output table (output = "table") as variables `suff_true` (cause is present) and
#'    `suff_false` (cause is absent).
#'  * A ratio is calculated using the following formula: `(suff_true / num_combos_true) / (suff_false / num_combos_false)`. In the output table
#'    (output = "table"), this value is stored in variable `ratio`. In the nice output (output = "nice"), it is reported in the column `Impact`,
#'    which shows: `ratio [suff_true/num_combos_true vs. suff_false/num_combos_false]`
#'  * There are two special cases when calculating the `ratio`. When `suff_true > 0` but `suff_false == 0`, the outcome only occurs if the
#'    corresponding component cause is present. The `ratio` then gets value `necessary`. When `suff_true == 0` and `suff_false == 0`, the `ratio`
#'    gets value `not a cause`.
#'
#' @param scc An object of class `epicmodel_scc`.
#' @param depends TRUE (default) or FALSE. If FALSE, only includes sufficient causes with suffciency status "always".
#' @param output A single element of type character, either "nice" (default) or "table". If "table", returns a data.frame. If "nice", a nicely
#' formated output is printed in the console.
#'
#' @returns Either a dataframe (`output` = "table") with one row for every component cause and with variables `id` (step ID), `desc` (step
#' description), `suff_true`, `suff_false`, `num_combos_true`, `num_combos_false`, and `ratio`, or a nicely formated output in the console
#' (`output` = "nice"). See Details for more information.
#'
#' @export
#'
#' @examples
#' # Create some SCC model
#' steplist_checked <- check_steplist(steplist_rain)
#' scc_model <- create_scc(steplist_checked)
#'
#' # Use the SCC model in effect_size()
#' effect_size(scc_model)
effect_size <- function(scc, depends = TRUE, output = c("nice","table")) {
  # Check inputs
  if (inherits(scc, "epicmodel_scc") %>% magrittr::not()) {
    cli::cli_abort("{.var scc} must be a {.emph epicmodel_scc} class object!",
                   class = "no_scc")
  }

  rlang::try_fetch(checkmate::assert_logical(depends, any.missing = F, len = 1, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var depends} must be TRUE or FALSE!",
                                    parent = cnd, class = "input_depends")
                   })

  if (scc$sc_status %>% magrittr::equals("always") %>% all_false() & !depends) {
    cli::cli_abort("{.var depends} cannot be FALSE if there are only sufficient causes with status 'depends'!",
                   class = "input_depends_false_no_always")
  }

  rlang::try_fetch(output <- match.arg(output),
                   error = function(cnd) {
                     cli::cli_abort("{.var output} must be one of the following strings: 'nice' or 'table'!",
                                    parent = cnd, class = "input_output")
                   })
  #=============================================================================
  # Get component causes and sufficient causes
  cc <- scc$sc_cc %>% colnames()
  sc <- scc %>% scc_cause_sets(output = "id", depends = depends)

  # Prepare output container
  out <- data.frame(id = cc, desc = rep(NA_character_, length(cc)), suff_true = rep(NA_integer_, length(cc)),
                    suff_false = rep(NA_integer_, length(cc)), num_combos_true = rep(NA_integer_, length(cc)),
                    num_combos_false = rep(NA_integer_, length(cc)),ratio = rep(NA_character_, length(cc)))

  # Fill output container
  cli::cli_progress_bar("Check impact of every component cause", total = nrow(out), type = "tasks")
  for (i in 1:nrow(out)) {
    ## Get all combinations of component causes
    grid <- vector(mode = "list", length = length(cc))
    names(grid) <- cc
    for (j in 1:length(grid)) {
      grid[[j]] <- c(TRUE,FALSE)
    }
    grid %<>% expand.grid(.)
    ## Exclude implausible combinations of component causes
    if (nrow(scc$steplist$icc) > 0) {
      grid$icc <- NA
      for (j in 1:nrow(scc$steplist$icc)) {
        for (k in 1:nrow(grid)) {
          if (grid[k,scc$steplist$icc[j,"id1"]] & grid[k,scc$steplist$icc[j,"id2"]]) {
            grid$icc[k] <- TRUE
          }
        }
      }
      grid %<>% dplyr::filter(is.na(.data$icc))
      grid$icc <- NULL
    }
    ## Create version with the component cause of interest always being TRUE
    grid_true <- grid %>% dplyr::filter(.data[[out$id[i]]] == TRUE)
    ## Create version with the component cause of interest always being FALSE
    grid_false <- grid %>% dplyr::filter(.data[[out$id[i]]] == FALSE)
    ## Create containers for sufficiency indicators
    suff_true <- rep(NA, nrow(grid_true))
    suff_false <- rep(NA, nrow(grid_false))
    ## Check sufficiency for every line in grid_true and grid_false
    for (j in 1:nrow(grid_true)) {
      ### Check grid_true
      temp_suff_true <- rep(NA,length(sc))
      for (k in 1:length(sc)) {
        temp_suff_true[k] <- sc[[k]] %>% magrittr::is_in(colnames(grid_true)[grid_true[j,] %>% t() %>% magrittr::extract(,1)]) %>% all_true()
      }
      suff_true[j] <- temp_suff_true %>% all_false() %>% magrittr::not()
    }
    for (j in 1:nrow(grid_false)) {
      ### Check grid_false
      temp_suff_false <- rep(NA,length(sc))
      for (k in 1:length(sc)) {
        temp_suff_false[k] <- sc[[k]] %>% magrittr::is_in(colnames(grid_false)[grid_false[j,] %>% t() %>% magrittr::extract(,1)]) %>% all_true()
      }
      suff_false[j] <- temp_suff_false %>% all_false() %>% magrittr::not()
    }
    ## Fill out
    out$num_combos_true[i] <- nrow(grid_true)
    out$num_combos_false[i] <- nrow(grid_false)
    out$suff_true[i] <- sum(suff_true)
    out$suff_false[i] <- sum(suff_false)
    out$desc[i] <- scc$steplist$step$desc_step[scc$steplist$step$id_step == out$id[i]]

    if (sum(suff_true) > 0 & sum(suff_false) == 0) {
      out$ratio[i] <- "necessary"
    }
    if (sum(suff_true) == 0 & sum(suff_false) == 0) {
      out$ratio[i] <- "not a cause"
    }
    if (sum(suff_false) > 0) {
      out$ratio[i] <- ((sum(suff_true) / nrow(grid_true)) / (sum(suff_false) / nrow(grid_false))) %>% round(2) %>% format(nsmall = 2)
    }
    cli::cli_progress_update()
  }
  cli::cli_alert_success(paste0(nrow(out),"/",nrow(out)," | Check impact of every component cause"))

  # Finalize output
  if (output == "table") {
    return(out)
  }
  if (output == "nice") {
    out$Impact <- paste0(out$ratio," [",out$suff_true,"/",out$num_combos_true," vs. ",out$suff_false,"/",out$num_combos_false,"]")
    out$sort <- NA_real_
    for (i in 1:nrow(out)) {
      if (out$ratio[i] == "not a cause") {out$sort[i] <- -1}
      if (out$ratio[i] == "necessary") {out$sort[i] <- 1e8}
      if (out$ratio[i] != "necessary" & out$ratio[i] != "not a cause") {out$sort[i] <- out$ratio[i] %>% as.numeric()}
    }
    out %<>% dplyr::arrange(dplyr::desc(sort))
    out$`Component Cause` <- out$desc %>% stringr::str_replace("^Start: ","")
    out %<>% dplyr::select(dplyr::all_of(c("Component Cause","Impact")))
    return(out)
  }
}

#' Do steps appear in sufficient causes?
#'
#' Extracts from a SCC model, if certain steps are part of the mechanism of sufficient causes. If you want a list of all steps, ignore argument
#' `steps`.
#'
#' @param scc An object of class `epicmodel_scc`.
#' @param steps A character vector containing step IDs. IF NULL (default), provides a list of all steps.
#' @param output A single element of type character, either "nice" (default) or "table". If "table", returns a list (or data.frame if steps = NULL).
#' If "nice", a nicely formated output is printed in the console.
#'
#' @returns Either a list (`output` = "table") with length equal to the number of sufficient causes and each element being a named vector of
#' TRUE/FALSE with the variables in `steps` as names and TRUE indicating that the step appears in the corresponding sufficient cause, or a nicely
#' formated output in the console (`output` = "nice"). If steps = NULL and output = "table", returns a data.frame, which contains variables
#' `id_step` and `desc_step` from the `epicmodel_steplist_checked` data.frame `step`.
#'
#' @export
#'
#' @examples
#' # Create some SCC model
#' steplist_checked <- check_steplist(steplist_rain)
#' scc_model <- create_scc(steplist_checked)
#'
#' # Check if one or more steps are part of the mechanism for each sufficient cause
#' sc_contain_steps(scc_model, c("THENa1","THENa5"))
sc_contain_steps <- function(scc, steps = NULL, output = c("nice","table")) {
  # Check inputs
  if (inherits(scc, "epicmodel_scc") %>% magrittr::not()) {
    cli::cli_abort("{.var scc} must be a {.emph epicmodel_scc} class object!",
                   class = "no_scc")
  }

  rlang::try_fetch(checkmate::assert_character(steps, any.missing = F, min.chars = 1, null.ok = T),
                   error = function(cnd) {
                     cli::cli_abort("{.var steps} must be a character vector!",
                                    parent = cnd, class = "input_steps")
                   })

  rlang::try_fetch(output <- match.arg(output),
                   error = function(cnd) {
                     cli::cli_abort("{.var output} must be one of the following strings: 'nice' or 'table'!",
                                    parent = cnd, class = "input_output")
                   })
  #=============================================================================
  # Print list of steps if steps is NULL
  if (is.null(steps)) {
    list_of_steps <- scc$steplist$step %>% dplyr::select(dplyr::all_of(c("id_step","desc_step")))
    list_of_steps$merge <- paste0(list_of_steps$id_step,": ",list_of_steps$desc_step)
    if (output == "nice") {
      return(cli::cli_ul(list_of_steps$merge))
    }
    if (output == "table") {
      return(list_of_steps[,c("id_step","desc_step")])
    }
  } else {
  # Continue when steps have been provided
    if (steps %>% magrittr::is_in(scc$steplist$step$id_step) %>% all_true() %>% magrittr::not()) {
      no_steps <- steps[steps %>% magrittr::is_in(scc$steplist$step$id_step) %>% magrittr::not()] %>% stringr::str_c(collapse = ", ")
      cli::cli_abort(c("Not all elements in {.var steps} are valid step IDs: {no_steps}",
                       "i" = "Run {.fn sc_contain_steps} without {.var steps} argument to list all available steps."),
                     class = "input_steps_exist")
    }

    # Get names and component causes of sufficient causes
    sc_names <- scc$sc_status %>% names()
    cc <- scc %>% scc_cause_sets(output = "desc_no_start", depends = T)

    # Create container
    step_info <- vector(mode = "list", length = length(sc_names)) %>% magrittr::set_names(sc_names)
    for (i in 1:length(step_info)) {
      temp <- rep(FALSE, length(steps)) %>% magrittr::set_names(steps)
      for (j in 1:length(steps)) {
        temp[j] <- names(temp)[j] %in% scc$sc_steps[[sc_names[i]]]
      }
      step_info[[sc_names[i]]] <- temp
    }

    if (output == "nice") {
    # Loop over every sufficient cause
      for (i in 1:length(sc_names)) {
        ## Header for sufficient cause
        cli::cli_h2(paste("SC",i))
        ## List component causes
        cli::cli_par()
        cli::cli_text("Component causes:")
        cli::cli_ul(cc[[sc_names[i]]])
        cli::cli_end()
        ## List results for steps
        for (j in 1:length(steps)) {
          if (step_info[[i]][j]) {
            cli::cli_alert_success(paste0("SC",i," contains step '",
                                          scc$steplist$step$desc_step[scc$steplist$step$id_step == names(step_info[[i]])[j]],
                                          "' (",names(step_info[[i]])[j],")"))
          } else {
            cli::cli_alert_danger(paste0("SC",i," does not contain step '",
                                         scc$steplist$step$desc_step[scc$steplist$step$id_step == names(step_info[[i]])[j]],
                                          "' (",names(step_info[[i]])[j],")"))
          }
        }
      }
    }
    if (output == "table") {
      return(step_info)
    }
  }
}

#' Extracting component causes from SCC model
#'
#' Extracting component causes by sufficient cause from an `epicmodel_scc` object.
#'
#' @param scc An object of class `epicmodel_scc`.
#' @param output A single element of type character, which determines the type of output. Options are "id", "desc", "desc_no_start", and "all".
#' See returns-part below for description.
#' @param depends TRUE (default) or FALSE. If FALSE, only includes sufficient causes with sc_status "always".
#' @param unknown TRUE or FALSE (default). If TRUE, unknown causes are added to the SCC model: every sufficient cause gets an additional
#' individual unknown component cause representing additional unknown components; an unknown sufficient cause is added to the model consisting of
#' a single unknown component cause and representing all unknown sufficient causes.
#'
#' @returns A named list but its content depends on parameter "output". The names correspond to the component cause set IDs, i.e., `cc[[:digit:]]+`.
#'  * id: Returns a named list of character vectors. Each vector contains the step IDs of its component causes.
#'  * desc: Returns a named list of character vectors. Each vector contains the step descriptions of its component causes.
#'  * desc_no_start: Returns a named list of character vectors. Each vector contains the step descriptions of its component causes, but with the
#'   "Start: " in the beginning removed.
#'  * all: A named list of the three lists above. The names correspond to the corresponding option for parameter "output".
#'
#' @export
#'
#' @examples
#' # Create some SCC model
#' steplist_checked <- check_steplist(steplist_rain)
#' scc_model <- create_scc(steplist_checked)
#'
#' # Get sets of component causes that form the sufficient causes
#' scc_cause_sets(scc_model, output = "all")
scc_cause_sets <- function(scc, output = c("id","desc","desc_no_start","all"), depends = TRUE, unknown = FALSE) {
  # Check input
  if (inherits(scc, "epicmodel_scc") %>% magrittr::not()) {
    cli::cli_abort("{.var scc} must be a {.emph epicmodel_scc} class object!",
                   class = "no_scc")
  }

  rlang::try_fetch(checkmate::assert_logical(depends, any.missing = F, len = 1, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var depends} must be TRUE or FALSE!",
                                    parent = cnd, class = "input_depends")
                   })

  if (scc$sc_status %>% magrittr::equals("always") %>% all_false() & !depends) {
    cli::cli_abort("{.var depends} cannot be FALSE if there are only sufficient causes with status 'depends'!",
                   class = "input_depends_false_no_always")
  }

  rlang::try_fetch(checkmate::assert_logical(unknown, any.missing = F, len = 1, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var unknown} must be TRUE or FALSE!",
                                    parent = cnd, class = "input_unknown")
                   })

  rlang::try_fetch(output <- match.arg(output),
                   error = function(cnd) {
                     cli::cli_abort("{.var output} must be one of the following strings: 'id', 'desc', 'desc_no_start', or 'all'!",
                                    parent = cnd, class = "input_output")
                   })
  #=============================================================================
  # Exclude depends if depends = F
  if (unknown == F) {
    if (depends == F) {
      sc <- scc$sc_cc[names(scc$sc_status[scc$sc_status == "always"]),]
    } else {
      sc <- scc$sc_cc
    }
  } else {
    if (depends == F) {
      sc <- scc$unknown_cc[names(scc$unknown_status[scc$unknown_status %in% c("always","unknown")]),]
    } else {
      sc <- scc$unknown_cc
    }
  }

  # Create container
  out_id <- vector(mode = "list", length = nrow(sc)) %>% magrittr::set_names(rownames(sc))
  out_desc <- vector(mode = "list", length = nrow(sc)) %>% magrittr::set_names(rownames(sc))
  out_desc_no_start <- vector(mode = "list", length = nrow(sc)) %>% magrittr::set_names(rownames(sc))

  # Get component causes of every
  for (i in 1:length(out_id)) {
    out_id[[i]] <- colnames(sc)[sc[names(out_id)[i],] %>% t() %>% magrittr::extract(,1)]

    temp_desc <- vector(mode = "character", length = length(out_id[[i]]))
    for (j in 1:length(temp_desc)) {
      desc_val <- scc$steplist$step$desc_step[scc$steplist$step$id_step == out_id[[i]][j]]
      if (length(desc_val) > 0) {
        temp_desc[j] <- desc_val
      } else {
        temp_desc[j] <- out_id[[i]][j]
      }
    }
    out_desc[[i]] <- temp_desc

    temp_desc_no_start <- temp_desc %>% stringr::str_replace("^Start: ","") %>% stringr::str_replace("^IFNOT.+THEN[:blank:]","")
    out_desc_no_start[[i]] <- temp_desc_no_start
  }

  # Return output
  if (output == "id") {
    out <- out_id
  }
  if (output == "desc") {
    out <- out_desc
  }
  if (output == "desc_no_start") {
    out <- out_desc_no_start
  }
  if (output == "all") {
    out <- list(id = out_id, desc = out_desc, desc_no_start = out_desc_no_start)
  }
  #=============================================================================
  # Check output
  if (output == "all") {
    rlang::try_fetch({
        checkmate::assert_list(out, any.missing = F, null.ok = F, len = 3, types = "list")
        checkmate::assert_set_equal(names(out), c("id","desc","desc_no_start"))
      }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                                 "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                               parent = cnd, class = "out")
    })

    rlang::try_fetch({
        checkmate::assert_list(out$id, any.missing = F, null.ok = F, min.len = 1, types = "character", names = "unique")
        checkmate::assert_character(names(out$id), pattern = "^cc[[:digit:]]+$")
      }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                                 "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                               parent = cnd, class = "out")
    })

    rlang::try_fetch({
        checkmate::assert_list(out$desc, any.missing = F, null.ok = F, min.len = 1, types = "character", names = "unique")
        checkmate::assert_character(names(out$desc), pattern = "^cc[[:digit:]]+$")
      }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                                 "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                               parent = cnd, class = "out")
    })

    rlang::try_fetch({
        checkmate::assert_list(out$desc_no_start, any.missing = F, null.ok = F, min.len = 1, types = "character", names = "unique")
        checkmate::assert_character(names(out$desc_no_start), pattern = "^cc[[:digit:]]+$")
      }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                                 "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                               parent = cnd, class = "out")
    })
  } else {
    rlang::try_fetch({
      checkmate::assert_list(out, any.missing = F, null.ok = F, min.len = 1, types = "character", names = "unique")
      checkmate::assert_character(names(out), pattern = "^cc[[:digit:]]+$")
    }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "out")
    })
  }
  #=============================================================================
  return(out)
}

#' Check if a certain set of component causes is suffcient
#'
#' Provide a SCC model and a set of component causes and evaluate if the provided set of causes fulfills any sufficient cause, i.e., is sufficient
#' for the outcome to occur based on the provided SCC model. Fulfilling a sufficient cause means that all component causes of a certain sufficient
#' cause are in the provided set of causes. Unknown causes are ignored by this function.
#'
#' @details Depending on the value of `type`, the following values are possible:
#'  * `type = "status"`: If the provided set of `causes` contains all component causes of a sufficient cause with status "always", returns "always".
#'    If the provided set of `causes` only fulfills sufficient cause with status "depends" or "depends (potential order implausibilities)", returns
#'    "depends". If no sufficient causes are fulfilled, returns "never".
#'  * `type = "binary"`: If the returned status would have been "always" or "depends", TRUE is returned. If the returned status would have been
#'    "never", returns FALSE.
#'
#' @param scc An object of class `epicmodel_scc`.
#' @param causes NULL (default) or a character vector containing IDs of a set of component causes. If NULL, prints a list of all available
#' component causes.
#' @param type Either "status" (default) or "binary". If "status", returns one of "always", "depends", "never". If "binary", returns TRUE or FALSE.
#'
#' @returns For `type = "binary`, returns TRUE if all component causes for at least one sufficient cause are in `causes` and FALSE otherwise. For
#' `type = status`, returns "always" if at least one sufficient cause with sufficiency status "always" is fulfilled. If not, returns "depends" if
#' at least one sufficient cause with sufficiency status "depends" or "depends (potential order implausibilities)" is fulfilled. If no sufficient
#' cause is fulfilled, returns "never".
#'
#' @export
#'
#' @examples
#' # Create some SCC model
#' steplist_checked <- check_steplist(steplist_rain)
#' scc_model <- create_scc(steplist_checked)
#'
#' # Check sufficiency for a certain set of component causes
#' are_sufficient(scc_model, c("THENa1","THENa5"), type = "status")
#' are_sufficient(scc_model, c("THENa1","THENa5"), type = "binary")
are_sufficient <- function(scc, causes = NULL, type = c("status","binary")) {
  # Check input
  if (inherits(scc, "epicmodel_scc") %>% magrittr::not()) {
    cli::cli_abort("{.var scc} must be a {.emph epicmodel_scc} class object!",
                   class = "no_scc")
  }

  rlang::try_fetch({
      checkmate::assert_character(causes, any.missing = F, null.ok = T, min.chars = 1, min.len = 1, unique = TRUE)
    }, error = function(cnd) {cli::cli_abort("{.var causes} must be a character vector (or NULL)!",
                                             parent = cnd, class = "input_causes")
  })

  rlang::try_fetch(type <- match.arg(type),
                   error = function(cnd) {
                     cli::cli_abort("{.var type} must be one of the following strings: 'status' or 'binary'!",
                                    parent = cnd, class = "input_type")
                   })
  #=============================================================================
  # Extract set of causes
  prc <- scc$steplist %>% process_steplist()
  split <- prc %>% split_prc()

  # With no causes specified
  if (is.null(causes)) {
    ## Get list of causes and combine ID and description
    list_of_steps <- split$causes %>% dplyr::left_join(scc$steplist$step[,c("id_step","desc_step")], by = "id_step") %>%
      dplyr::select(dplyr::all_of(c("id_step","desc_step")))
    list_of_steps$merge <- paste0(list_of_steps$id_step,": ",list_of_steps$desc_step %>% stringr::str_replace("Start: ",""))
    ## Return list of causes
    if (nrow(list_of_steps) == 0) {
      return(cli::cli_alert_info("There are no causes!"))
    } else {
      return(cli::cli_ul(list_of_steps$merge))
    }
  } else {
    # With causes specified
    ## Check if selected causes exist
    if (causes %>% magrittr::is_in(split$causes$id_step) %>% all_true() %>% magrittr::not()) {
      invalid_causes <- causes[causes %>% magrittr::is_in(split$causes$id_step) %>% magrittr::not()] %>%
        stringr::str_c(collapse = ", ")
      cli::cli_abort(c("All elements of {.var causes} must be IDs of valid component causes!",
                       "i" = "The following component causes are invalid: {invalid_causes}",
                       "i" = "Run {.fn which_intervention} without {.var causes} argument to list all available component causes."),
                     class = "invalid_causes")
    }
  }

  # Get component cause IDs by sufficient cause
  cc_id <- scc %>% scc_cause_sets(output = "id", depends = T)

  # Check sufficient causes against 'causes'
  suff_causes <- rep(NA, length(cc_id)) %>% magrittr::set_names(names(cc_id))
  for (i in 1:length(suff_causes)) {
    suff_causes[i] <- cc_id[[names(suff_causes)[i]]] %>% magrittr::is_in(causes) %>% all_true()
  }
  if (type == "binary") {
    if (suff_causes %>% all_false()) {
      return(FALSE)
      } else {
      return(TRUE)
      }
    }
  if (type == "status") {
    if (suff_causes %>% all_false()) {
      out <- "never"
    } else {
      suff_status <- scc$sc_status[names(suff_causes)[suff_causes]]
      if ("always" %>% magrittr::is_in(suff_status) %>% all_false() %>% magrittr::not()) {
        out <- "always"
      } else {
        out <- "depends"
      }
    }
    return(out)
  }
}

#' Show all steps of a SCC model
#'
#' Prints all steps that are part of a sufficient-component cause model. The function wraps [sc_contain_steps()] with `steps = NULL`.
#'
#' @param scc An object of class `epicmodel_scc`.
#' @param output A single element of type character, either "nice" (default) or "table". If "table", returns a data.frame. If "nice", a nicely
#' formated output is printed in the console.
#'
#' @returns Either a data.frame (`output` = "table") with variables `id_step` (step ID) and `desc_step` (step description) and one row for every
#' step in the model, i.e., from the `epicmodel_steplist_checked` data.frame `step`, or a nicely formated output in the console (`output` = "nice").
#'
#' @export
#'
#' @examples
#' # Create some SCC model
#' steplist_checked <- check_steplist(steplist_rain)
#' scc_model <- create_scc(steplist_checked)
#'
#' # Show all steps
#' show_steps(scc_model)
show_steps <- function(scc, output = c("nice", "table")) {
  # Check inputs
  if (inherits(scc, "epicmodel_scc") %>% magrittr::not()) {
    cli::cli_abort("{.var scc} must be a {.emph epicmodel_scc} class object!",
                   class = "no_scc")
  }

  rlang::try_fetch(output <- match.arg(output),
                   error = function(cnd) {
                     cli::cli_abort("{.var output} must be one of the following strings: 'nice' or 'table'!",
                                    parent = cnd, class = "input_output")
                   })
  #=============================================================================
  # Table output
  if (output == "table") {
    out <- sc_contain_steps(scc = scc, output = "table")
    return(out)
  }
  if (output == "nice") {
    sc_contain_steps(scc = scc, output = "nice")
  }
}
