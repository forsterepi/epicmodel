#' Split processed steplist
#'
#' Splits the processed step list into component causes, interventions, non-starting steps, and IFNOT steps.
#'
#' @details Definitions:
#' * Starting steps: Steps without IF condition
#' * Component causes: Starting steps which appear in other IF conditions (and maybe additionally in IFNOT conditions)
#' * Interventions: Starting steps which do not appear in IF conditions but only in IFNOT conditions
#' * Non-starting steps: Steps with IF condition
#' * IFNOT Steps: Steps with IFNOT condition, including starting steps with IFNOT condition
#' * End steps: Steps with end_step == "1". They are usually non-starting steps. Only end steps are used in outcome definitions.
#' Therefore, component causes, interventions, and non-starting steps are mutually exclusive and together form the complete list of steps.
#'
#' @param prc A processed steplist created with `process_steplist()`.
#'
#' @returns A list of length five containing the following elements:
#' * non_start_steps: A tibble with similar structur to `prc`. In fact, a subset of `prc`. If empty, the same tibble with 0 rows.
#' * causes: A tibble with similar structur to `prc`. In fact, a subset of `prc`. Cannot be empty.
#' * interventions: A tibble with similar structur to `prc`. In fact, a subset of `prc`. If empty, the same tibble with 0 rows.
#' * ifnot_steps: A character vector. If empty, a character vector of length 0.
#' * end_steps: A character vector. If empty, a character vector of length 0.
#'
#' @noRd
split_prc <- function(prc) {
  # Check input
  rlang::try_fetch({
      checkmate::assert_tibble(prc, null.ok = F, ncols = 12, min.rows = 1, col.names = "unique")
      checkmate::assert_subset(colnames(prc), c("id_step","then_step","subject_step","does_step","object_step","where_step","if_step","if_list",
                                                "ifnot_step","ifnot_list","end_step","module_step"), empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var prc}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_prc")
  })
  #=============================================================================
  # Extract steps with IF condition
  non_start_steps <- prc %>% dplyr::filter(!is.na(.data$if_step))

  # Extract interventions
  if_vars <- prc$if_list %>% purrr::map_dfr(as.data.frame) %>% magrittr::extract2("id") %>% unique() %>% .[!is.na(.)]
  ifnot_vars <- prc$ifnot_list %>% purrr::map_dfr(as.data.frame) %>% magrittr::extract2("id") %>% unique() %>% .[!is.na(.)]
  interventions <- prc %>% dplyr::filter(is.na(.data$if_step)) %>% dplyr::filter((.data$then_step %in% ifnot_vars) & !(.data$then_step %in% if_vars))

  # Extract component causes
  causes <- prc %>% dplyr::filter(is.na(.data$if_step)) %>% dplyr::filter(!(.data$id_step %in% interventions$id_step))

  # Extract steps with IFNOT condition
  ifnot_steps <- prc %>% dplyr::filter(!is.na(.data$ifnot_step)) %>% magrittr::extract2("id_step")

  # Extract steps that are marked as end steps
  end_steps <- prc %>% dplyr::filter(.data$end_step == "1") %>% magrittr::extract2("id_step")

  # Create out
  out <- list(non_start_steps = non_start_steps, causes = causes, interventions = interventions,
              ifnot_steps = ifnot_steps, end_steps = end_steps)
  #=============================================================================
  # Check output
  rlang::try_fetch({
      checkmate::assert_list(out, any.missing = F, null.ok = F, len = 5, names = "unique")
      checkmate::assert_subset(names(out), c("non_start_steps","causes","interventions","ifnot_steps","end_steps"), empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var out}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output_out")
  })
  ## causes cannot be empty
  rlang::try_fetch({
      checkmate::assert_tibble(out$causes, null.ok = F, ncols = 12, min.rows = 1, col.names = "unique")
      checkmate::assert_subset(colnames(out$causes), colnames(prc), empty.ok = F)
      checkmate::assert_subset(out$causes$id_step, prc$id_step, empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var out$causes}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output_out$causes")
  })
  ## other elements can be empty
  rlang::try_fetch({
      checkmate::assert_tibble(out$non_start_steps, null.ok = F, ncols = 12, col.names = "unique")
      checkmate::assert_subset(colnames(out$non_start_steps), colnames(prc), empty.ok = F)
      checkmate::assert_subset(out$non_start_steps$id_step, prc$id_step)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var out$non_start_steps}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output_out$non_start_steps")
  })

  rlang::try_fetch({
      checkmate::assert_tibble(out$interventions, null.ok = F, ncols = 12, col.names = "unique")
      checkmate::assert_subset(colnames(out$interventions), colnames(prc), empty.ok = F)
      checkmate::assert_subset(out$interventions$id_step, prc$id_step)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var out$interventions}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output_out$interventions")
  })

  rlang::try_fetch({
      checkmate::assert_character(out$ifnot_steps, any.missing = F, null.ok = F, min.chars = 1, unique = T)
      if (length(out$ifnot_steps) > 0) {
        checkmate::assert_subset(out$ifnot_steps, prc$id_step, empty.ok = F)
      }
    }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var out$ifnot_steps}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output_out$ifnot_steps")
  })

  rlang::try_fetch({
      checkmate::assert_character(out$end_steps, any.missing = F, null.ok = F, min.chars = 1, unique = T)
      if (length(out$end_steps) > 0) {
        checkmate::assert_subset(out$end_steps, prc$id_step, empty.ok = F)
      }
    }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var out$end_steps}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output_out$end_steps")
  })
  #=============================================================================
  return(out)
}

#' Get potential sufficient causes
#'
#' Used in `create_scc()`. Gets all valid combinations of component causes. Interventions are not considered as component causes. Invalid
#' combinations are:
#' * Combinations that contain incompatible component causes as specified in the ICC part of the steplist
#' * The combination with all component causes absent, i.e., FALSE.
#'
#' @param causes A tibble. Corresponds to element `causes` created by `split_prc()`, which is a subset of the processed steplist created by
#' `process_steplist()`.
#' @param steplist An object of class `epicmodel_steplist_checked`.
#'
#' @returns A data.frame with colnames equal to the step IDs of the component causes and rownames of format ^cc[[:digit:]]+$. Contains only TRUE or
#' FALSE and no missings.
#'
#' @noRd
get_cause_combinations <- function(causes, steplist) {
  # Check inputs
  if (inherits(steplist, "epicmodel_steplist_checked") %>% magrittr::not()) {
    cli::cli_abort(c("Input validation error: {.var steplist}",
                     "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"), class = "input_steplist")
  }

  rlang::try_fetch({
    checkmate::assert_tibble(causes, null.ok = F, ncols = 12, min.rows = 1, col.names = "unique")
    checkmate::assert_subset(colnames(causes), c("id_step","then_step","subject_step","does_step","object_step","where_step","if_step","if_list",
                                                 "ifnot_step","ifnot_list","end_step","module_step"), empty.ok = F)
    checkmate::assert_subset(causes$id_step, steplist$step$id_step, empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var causes}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_causes")
  })
  #=============================================================================
  # Create all combinations of component causes
  ## Empty list with one element per component cause
  grid <- vector(mode = "list", length = nrow(causes))
  names(grid) <- causes$id_step
  ## Include TRUE and FALSE as options for every component cause
  for (i in 1:length(grid)) {
    grid[[i]] <- c(TRUE,FALSE)
  }
  ## Expand the grid to a data.frame that contains all combinations
  out <- expand.grid(grid)

  # Exclude the combination with all component causes as FALSE (i.e., no component cause is present)
  out %<>% dplyr::filter(rowSums(.) > 0)

  # Exclude incompatible component causes
  if (nrow(steplist$icc) > 0) {
    out$icc <- NA
    for (i in 1:nrow(steplist$icc)) {
      for (j in 1:nrow(out)) {
        if ((steplist$icc[i,"id1"] %in% colnames(out)) & (steplist$icc[i,"id2"] %in% colnames(out))) {
          if (out[j,steplist$icc[i,"id1"]] & out[j,steplist$icc[i,"id2"]]) {
            out$icc[j] <- TRUE
          }
        }
      }
    }
    out %<>% dplyr::filter(is.na(.data$icc))
    out$icc <- NULL
  }

  # Rearrange rows to start with lower numbers of present component causes
  out %<>% dplyr::arrange(rowSums(.))

  # Add rownames of format ^cc[[:digit:]]+$, which are used to identify sets of component causes and sufficient causes throughout the functions
  rownames(out) <- paste0("cc",c(1:nrow(out)))
  #=============================================================================
  # Check output
  rlang::try_fetch({
      checkmate::assert_data_frame(out, types = "logical", any.missing = F, null.ok = F, ncols = nrow(causes), min.rows = 1,
                                   col.names = "unique", row.names = "unique")
      checkmate::assert_subset(colnames(out), causes$id_step, empty.ok = F)
      checkmate::assert_character(rownames(out), pattern = "^cc[[:digit:]]+$")
    }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output")
  })
  #=============================================================================
  return(out)
}

#' Transform outcome definition
#'
#' Used in `create_scc()` and `intervene()`. Transforms outcome definition to a format that can be used in `create_scc()`, which mimicks the
#' format of IF/IFNOT condition lists.
#'
#' @param steplist An object of type `epicmodel_steplist_checked`. Data.frame `outc` containing outcome definitions cannot be empty.
#'
#' @returns A data.frame with two columns, `sce` and `id`, which are both of type character. `sce` contains integers (as.character) and `id`
#' contains THEN statements of end steps.
#'
#' @noRd
transform_outc <- function(steplist) {
  # Check input
  if (inherits(steplist, "epicmodel_steplist_checked") %>% magrittr::not()) {
    cli::cli_abort(c("Input validation error: {.var steplist}",
                     "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"), class = "input_steplist")
  }

  rlang::try_fetch({
      checkmate::assert_character(steplist$outc$id_outc, min.len = 1, any.missing = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var steplist}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_steplist_outc_empty")
  })
  #=============================================================================
  # Prepare empty container
  outc_lines <- nrow(steplist$outc)
  out <- vector(mode = "list", length = outc_lines)

  # Fill empty container
  for (i in 1:outc_lines) {
    ## Split the i-th element into its components (THEN segments), which are combined by AND logic (+)
    temp <- steplist$outc$id_outc[i] %>% stringr::str_split("\\+") %>% magrittr::extract2(1)
    ## Create container for the i-th outcome definition
    out_temp <- matrix(rep(NA, length(temp)*2), nrow = length(temp), ncol = 2) %>%
      as.data.frame() %>%
      magrittr::set_colnames(c("sce","id"))
    ## Scenario (sce) gets the number i
    out_temp$sce <- as.character(i)
    ## Separated THEN statements become the ids
    out_temp$id <- temp
    ## Put temporary container for the i-th element into the overall container
    out[[i]] <- out_temp
  }

  # Paste all data.frames in individual list elements together to one data.frame
  out %<>% purrr::map_dfr(as.data.frame)
  #=============================================================================
  # Check output
  end_then <- steplist %>% process_steplist() %>% dplyr::filter(.data$end_step == "1") %>% magrittr::extract2("then_step")
  rlang::try_fetch({
      checkmate::assert_data_frame(out, types = "character", any.missing = F, null.ok = F, ncols = 2, min.rows = 1, col.names = "unique")
      checkmate::assert_subset(colnames(out), c("sce","id"), empty.ok = F)
      checkmate::assert_integerish(out$sce %>% as.numeric(), lower = 1, any.missing = F, null.ok = F)
      checkmate::assert_subset(out$id, end_then, empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output")
  })
  #=============================================================================
  return(out)
}

#' Creates as string the code for checking if IF conditions are fulfilled
#'
#' Used in `is_fulfilled()`.
#'
#' @param if_list See `is_fulfilled()`.
#'
#' @returns A single element of type character.
#'
#' @noRd
transform_if_list <- function(if_list) {
  # Check input
  rlang::try_fetch({
      checkmate::assert_data_frame(if_list, types = "character", any.missing = F, null.ok = F, ncols = 2, min.rows = 1, col.names = "unique")
      checkmate::assert_subset(colnames(if_list), c("sce","id"), empty.ok = F)
      checkmate::assert_integerish(if_list$sce %>% as.numeric(), lower = 1, null.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var if_list}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_if_list")
  })
  #=============================================================================
  # Get maximum number of scenarios
  max_sce <- if_list$sce %>% as.numeric() %>% max()

  # Create empty container
  out <- vector(mode = "character", length = max_sce)

  # Create code (as string) for evaluating each scenario
  for (i in 1:max_sce) {
    out[i] <- if_list %>% dplyr::filter(.data$sce == i) %>% magrittr::extract2("id") %>% stringr::str_c(collapse = "','") %>%
      paste0("c('",.,"') %>% magrittr::is_in(current_list_then) %>% magrittr::not() %>% sum() %>% magrittr::equals(0)")
  }

  # Collapse code (as string) from different scenarios
  out %<>% stringr::str_c(collapse = " | ")
  #=============================================================================
  # Check output
  rlang::try_fetch({
      checkmate::assert_character(out, any.missing = F, null.ok = F, len = 1, min.chars = 1)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output")
  })
  #=============================================================================
  return(out)
}

#' Creates as string the code for checking if the outcome condition is fulfilled
#'
#' Used in `is_fulfilled_outc`.
#'
#' @param outc_list See `is_fulfilled_outc`.
#'
#' @returns A single element of type character.
#'
#' @noRd
transform_outc_list <- function(outc_list) {
  # Check input
  rlang::try_fetch({
      checkmate::assert_data_frame(outc_list, types = "character", any.missing = F, null.ok = F, ncols = 2, min.rows = 1, col.names = "unique")
      checkmate::assert_subset(colnames(outc_list), c("sce","id"), empty.ok = F)
      checkmate::assert_integerish(outc_list$sce %>% as.numeric(), lower = 1, any.missing = F, null.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_outc_list")
  })
  #=============================================================================
  # Get maximum number of scenarios
  max_sce <- outc_list$sce %>% as.numeric() %>% max()

  # Create empty container
  out <- vector(mode = "character", length = max_sce)

  # Create code (as string) for evaluating each scenario
  for (i in 1:max_sce) {
    out[i] <- outc_list %>% dplyr::filter(.data$sce == i) %>% magrittr::extract2("id") %>% stringr::str_c(collapse = "','") %>%
      paste0("c('",.,"') %>% magrittr::is_in(final_list_then) %>% magrittr::not() %>% sum() %>% magrittr::equals(0)")
  }

  # Collapse code (as string) from different scenarios
  out %<>% stringr::str_c(collapse = " | ")
  #=============================================================================
  # Check output
  rlang::try_fetch({
      checkmate::assert_character(out, any.missing = F, null.ok = F, len = 1, min.chars = 1)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output")
  })
  #=============================================================================
  return(out)
}

#' Check if condition is fulfilled
#'
#' Used in `next_round_of_steps()`, `check_ifnot()`, and `get_prevented_causes()`.
#'
#' @param if_list A data.frame with two columns, `sce` and `id`, which are both of type character. `sce` contains integers (as.character) and `id`
#' contains THEN statements of end steps.
#' @param current_list_then A character vector containing THEN statements. When used in `next_round_of_steps()`, `current_list_then` has been
#' created in `is_sufficient()`. When used in `check_ifnot()`, it contains the THEN statements of the final list of steps of a certain sufficient
#' cause. When used in `get_prevented_causes()`, it contains THEN statements of selected interventions.
#'
#' @returns TRUE or FALSE.
#'
#' @noRd
is_fulfilled <- function(if_list, current_list_then) {
  ## Check input
  rlang::try_fetch({
      checkmate::assert_data_frame(if_list, types = c("character", "logical"), null.ok = F, ncols = 2, min.rows = 1, col.names = "unique")
      checkmate::assert_subset(colnames(if_list), c("sce","id"), empty.ok = F)
      checkmate::assert_integerish(if_list$sce %>% as.numeric(), lower = 1, null.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var if_list}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_if_list")
  })

  rlang::try_fetch({
      checkmate::assert_character(current_list_then, any.missing = F, null.ok = F, min.len = 1, min.chars = 1, unique = T)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var current_list_then}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_current_list_then")
  })
  #=============================================================================
  # Checks if if_list is empty, i.e., only one row with both values NA (and the columns of type logical)
  if (is.na(if_list[1,2])) {
    ## If empty, the condition (no condition) is automatically fulfilled
    out <- TRUE
  } else {
    ## Otherwise, check condition created bytransform_if_list()
     out <- eval(parse(text = transform_if_list(if_list)))
  }
  #=============================================================================
  #Check output
  rlang::try_fetch({
      checkmate::assert_logical(out, any.missing = F, len = 1, null.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output")
  })
  #=============================================================================
  return(out)
}

#' Check if list of steps fulfills outcome definition
#'
#' Used in `is_sufficient()`.
#'
#' @param outc_list See `is_sufficient()`.
#' @param final_list_then A character vector contaning the THEN statements of the final list of steps that can be caused by a certain set of
#' component causes, created by `is_sufficient()`.
#'
#' @returns TRUE or FALSE.
#'
#' @noRd
is_fulfilled_outc <- function(outc_list, final_list_then) {
  # Check input
  rlang::try_fetch({
      checkmate::assert_data_frame(outc_list, types = "character", any.missing = F, null.ok = F, ncols = 2, min.rows = 1, col.names = "unique")
      checkmate::assert_subset(colnames(outc_list), c("sce","id"), empty.ok = F)
      checkmate::assert_integerish(outc_list$sce %>% as.numeric(), lower = 1, any.missing = F, null.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_outc_list")
  })

  rlang::try_fetch({
      checkmate::assert_character(final_list_then, any.missing = F, null.ok = F, min.len = 1, min.chars = 1, unique = T)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var final_list_then}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_final_list_then")
  })
  #=============================================================================
  out <- eval(parse(text = transform_outc_list(outc_list)))
  #=============================================================================
  # Check output
  rlang::try_fetch({
      checkmate::assert_logical(out, any.missing = F, len = 1, null.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output")
  })
  #=============================================================================
  return(out)
}

#' Check IF condition for a single loop in `is_sufficient()`
#'
#' Used in `is_sufficient()`.
#'
#' @param steps_left A tibble, which is a subset of processed data created by `process_steplist()`. `steps_left` is created in `is_sufficient()`
#' from `non_start_steps`.
#' @param current_list_then A character vector containing THEN segments and created in `is_sufficient()`.
#'
#' @returns A character vector with step IDs.
#'
#' @noRd
next_round_of_steps <- function(steps_left, current_list_then) {
  # Check inputs
  rlang::try_fetch({
      checkmate::assert_tibble(steps_left, null.ok = F, ncols = 12, col.names = "unique")
      checkmate::assert_subset(colnames(steps_left), c("id_step","then_step","subject_step","does_step","object_step", "where_step","if_step",
                                                       "if_list","ifnot_step","ifnot_list","end_step","module_step"), empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var steps_left}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_steps_left")
  })

  rlang::try_fetch({
      checkmate::assert_character(current_list_then, any.missing = F, null.ok = F, min.len = 1, min.chars = 1, unique = T)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var current_list_then}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_current_list_then")
  })
  #=============================================================================
  # Add variable to tibble steps_left
  steps_left$fulfilled <- NA

  # Checks for remaining steps if their IF condition is fulfilled based on current_list_then
  if (nrow(steps_left) > 0) {
    for (k in 1:nrow(steps_left)) {
      steps_left$fulfilled[k] <- is_fulfilled(if_list = steps_left$if_list[k][[1]], current_list_then)
    }
  }

  # Gets IDs for steps with fulfilled IF condition
  out <- steps_left %>% dplyr::filter(.data$fulfilled == TRUE) %>% magrittr::extract2("id_step")
  #=============================================================================
  # Check output
  rlang::try_fetch({
      checkmate::assert_character(out, any.missing = F, null.ok = F, min.chars = 1, unique = T)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output")
  })
  #=============================================================================
  return(out)
}

#' Check sufficiency
#'
#' Used in `create_scc()`, `check_ifnot()`, and `check_causes_x_intv()`. In `create_scc()`, checks if a certain set of component causes can cause
#' enough steps to fulfill the outcome definition. IFNOT conditions are still ignored at this point. In `check_ifnot()`, checks if a certain set
#' of component causes, potentially after turning some components, which have been prevneted by IFNOT condiitons, to FALSE.
#'
#' @param cc In `create_scc()`, a data.frame with colnames equal to the step IDs of the component causes and rownames of format ^cc[[:digit:]]+$.
#' Contains only TRUE or FALSE and no missings. It is the output of function `get_cause_combinations()`. In `check_ifnot()`, the format is the same,
#' but it contains only a single row, namely the one to check.
#' @param row An integer, indicating the row of `cc`. In `create_scc()`, the index of a for-loop is used for `row`. In `check_ifnot()`, `row` is
#' hard-coded to `1`, since `cc` only has one row there.
#' @param non_start_steps In `create_scc()`, a tibble, which is a subset of processed data created by `process_steplist()`. The subsetting has been
#' done with `split_prc()`. The element used here is subset `non_start_steps`. In `check_ifnot()`, the format is similar, but individual rows have
#' been removed, namely the rows that are prevented by IFNOT conditions.
#' @param outc_list A data.frame with two columns, `sce` and `id`, which are both of type character. `sce` contains integers (as.character) and `id`
#' contains THEN statements of end steps. `outc_list` has been created by `transform_outc()`. It's the same in both `create_scc()` and
#' `check_ifnot()`.
#'
#' @returns A list with two elements: `is_suff` is either TRUE or FALSE, and `final_list` is a character vector.
#'
#' @noRd
is_sufficient <- function(cc, row, non_start_steps, outc_list) {
  # Check input
  rlang::try_fetch({
      checkmate::assert_data_frame(cc, types = "logical", any.missing = F, null.ok = F, min.cols = 1, min.rows = 1,
                                   col.names = "unique", row.names = "unique")
      checkmate::assert_character(rownames(cc), pattern = "^cc[[:digit:]]+$")
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var cc}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_cc")
  })

  rlang::try_fetch({
      checkmate::assert_integerish(row, lower = 1, any.missing = F, null.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var row}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_row")
  })

  rlang::try_fetch({
      checkmate::assert_tibble(non_start_steps, null.ok = F, ncols = 12, col.names = "unique")
      checkmate::assert_subset(colnames(non_start_steps), c("id_step","then_step","subject_step","does_step","object_step","where_step","if_step",
                                                            "if_list","ifnot_step","ifnot_list","end_step","module_step"), empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var non_start_steps}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_non_start_steps")
  })

  rlang::try_fetch({
      checkmate::assert_data_frame(outc_list, types = "character", any.missing = F, null.ok = F, ncols = 2, min.rows = 1, col.names = "unique")
      checkmate::assert_subset(colnames(outc_list), c("sce","id"), empty.ok = F)
      checkmate::assert_integerish(outc_list$sce %>% as.numeric(), lower = 1, any.missing = F, null.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error {.var outc_list}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_outc_list")
  })
  #=============================================================================
  # Start with the list of component causes
  new_steps <- colnames(cc)[cc[row,] %>% t() %>% magrittr::extract(,1)]
  current_list <- c()

  # Iterate to get all steps that can be causesd by the current set of component causes
  while (length(new_steps) > 0) {
    ## Add the newly added steps from the last iteration to the current list of steps
    current_list <- c(current_list,new_steps)
    ## Recalculate, which steps are not in the current list yet
    steps_left <- non_start_steps %>% dplyr::filter(!(.data$id_step %in% current_list))
    ## Get the THEN statement of the current list of steps
    current_list_then <- current_list %>% sep_step() %>% magrittr::extract2("then")
    ## Check for all remaining steps if the IF condition is fulfilled by the current list of steps
    new_steps <- next_round_of_steps(steps_left, current_list_then)
  }

  # Get final list of steps
  final_list <- current_list
  ## Only take THEN statements
  final_list_then <- final_list %>% sep_step() %>% magrittr::extract2("then")

  # Check if outcome definition is fulfilled by final list of steps. If yes => sufficient
  is_suff <- is_fulfilled_outc(outc_list,final_list_then)

  # Prepare output
  out <- list(is_suff = is_suff, final_list = final_list)
  #=============================================================================
  # Check output
  rlang::try_fetch({
      checkmate::assert_list(out, any.missing = F, null.ok = F, len = 2, names = "unique")
      checkmate::assert_subset(names(out), c("is_suff","final_list"), empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var out}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output_out")
  })

  rlang::try_fetch({
      checkmate::assert_logical(out$is_suff, any.missing = F, len = 1, null.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var out$is_suff}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output_out$is_suff")
  })

  rlang::try_fetch({
      checkmate::assert_character(out$final_list, any.missing = F, null.ok = F, min.len = 1, min.chars = 1, unique = T)
      checkmate::assert_subset(out$final_list, c(colnames(cc), non_start_steps$id_step), empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var out$final_list}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output_out$final_list")
  })
  #=============================================================================
  return(out)
}

#' Minimize list of sufficient causes
#'
#' Cut from the list of sufficient sets of component causes those that have sufficient subsets. Used in `create_scc()`.
#'
#' @details A sufficient set of component causes is minimal, if there does not exist a smaller set. A smaller set contains no additional component
#' causes compared to the larger set, but the larger set contains some component cause that are not part of the smaller set. Therefore, minimality
#' is checked via two criteria:
#' * Are all component causes, which are absent from set 1 also absent from set 2?
#' * Does set 2 contain less component causes than set 1?
#' If both is TRUE, set 2 is smaller than set 1, and therefore, set 1 is not minimal, since at least one smaller set exists. In the code, set 1
#' corresponds to the i-th set and set 2 corresponds to the j-th set. If the j-th set fulfills both criteria, minimality for the i-th set is changed
#' to FALSE.
#'
#' @param sc A data.frame with colnames equal to the step IDs of the component causes and rownames of format ^cc[[:digit:]]+$. Contains only TRUE or
#' FALSE and no missings. It is a subset of the output of function `get_cause_combinations()` and cut to sets of component causes that fulfill the
#' outcome condition, i.e., are sufficient.
#'
#' @returns An object similar to input `sc`, but with less rows. (If all sets were minimal, returns exactly input `sc`.)
#'
#' @noRd
minimize_sc <- function(sc) {
  # Check input
  rlang::try_fetch({
      checkmate::assert_data_frame(sc, types = "logical", any.missing = F, null.ok = F, min.cols = 1, min.rows = 1,
                                   col.names = "unique", row.names = "unique")
      checkmate::assert_character(rownames(sc), pattern = "^cc[[:digit:]]+$")
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var sc}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_sc")
  })
  #=============================================================================
  # Get number of sufficient sets of component causes
  n_rows <- nrow(sc)

  # Create empty container
  minimal <- rep(TRUE, n_rows) %>% magrittr::set_names(rownames(sc))

  # Loop over all sufficient sets of component causes
  cli::cli_progress_bar("Check if sufficient cause is minimal", total = n_rows, type = "tasks")
  for (i in 1:n_rows) {
    ## Which component causes are FALSE for the i-th set
    left_out_start <- colnames(sc)[which(sc[i, ] == F)]
    ## Loop over all sufficient sets of component causes (to compare with the i-th set)
    for (j in 1:n_rows) {
      ### Check if the component causes that are missing in the i-th set, are also missing in the j-th set
      if (length(left_out_start) > 0) {
        also_false <- sc[j,left_out_start] %>% as.logical() %>% all_false()
      } else {
        also_false <- T
      }
      ### Check if the j-th set contains less component causes than the i-th set
      less_starts <- sc[j,] %>% sum() %>% magrittr::is_less_than(sc[i, ] %>% sum())
      ### If both is TRUE for any j-th set, the i-th set is not minimal, i.e., at least on of the j-th sets is smaller
      if (also_false & less_starts) {
        minimal[rownames(sc)[i]] <- FALSE
      }
    }
    cli::cli_progress_update()
  }

  # Subset sc to minimal sets
  # LEGACY: sc <- sc[minimal,]
  sc %<>% dplyr::filter(rownames(sc) %in% names(minimal)[minimal])
  #=============================================================================
  # Check output
  rlang::try_fetch({
    checkmate::assert_data_frame(sc, types = "logical", any.missing = F, null.ok = F, min.cols = 1, min.rows = 1,
                                 col.names = "unique", row.names = "unique")
    checkmate::assert_character(rownames(sc), pattern = "^cc[[:digit:]]+$")
  }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var sc}",
                                             "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                           parent = cnd, class = "output_sc")
  })
  #=============================================================================
  cli::cli_alert_success(paste0(n_rows,"/",n_rows," | Check if sufficient cause is minimal"))
  cli::cli_alert_info(paste0(nrow(sc),"/",n_rows," sufficient causes are minimal"))
  return(sc)
}

#' Get sufficient causes with IFNOT
#'
#' Gets list of sufficient causes that need to be re-evaluated for IFNOT conditions. Used in `create_scc()` and `check_causes_x_intv()`.
#'
#' @param sc A data.frame with colnames equal to the step IDs of the component causes and rownames of format ^cc[[:digit:]]+$. Contains only TRUE or
#' FALSE and no missings. It is a subset of the output of function `get_cause_combinations()` and cut to sets of component causes that fulfill the
#' outcome condition, i.e., are sufficient.
#' @param sc_final_steps A named list of the same length as the number of rows of `sc`, i.e., the number of sufficient sets of component causes.
#' The names of `sc_final_steps` are the same as the rownames of `sc`, i.e., the names of the sufficient sets of component causes. The list elements
#' are character vectors containing the final list of IDs of all steps included in the corresponding sufficient cause.
#' @param ifnot_steps A character vector containing the IDs of steps with IFNOT condition.
#'
#' @returns A subset of `sc`, potentially with 0 rows.
#'
#' @noRd
get_sc_to_check_for_ifnot <- function(sc, sc_final_steps, ifnot_steps){
  # Check input
  rlang::try_fetch({
      checkmate::assert_data_frame(sc, types = "logical", any.missing = F, null.ok = F, min.cols = 1, min.rows = 1,
                                   col.names = "unique", row.names = "unique")
      checkmate::assert_character(rownames(sc), pattern = "^cc[[:digit:]]+$")
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var sc}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_sc")
  })

  rlang::try_fetch({
      checkmate::assert_list(sc_final_steps, any.missing = F, null.ok = F, min.len = 1, types = "character", names = "unique")
      checkmate::assert_character(names(sc_final_steps), pattern = "^cc[[:digit:]]+$")
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var sc_final_steps}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_sc_final_steps")
  })

  if (rownames(sc) %>% magrittr::equals(names(sc_final_steps)) %>% all_true() %>% magrittr::not()) {
    cli::cli_abort(c("Input validation error: {.var sc_final_steps}",
                     "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                   class = "input_names_sc_and_sc_final_steps")
  }

  rlang::try_fetch({
      checkmate::assert_character(ifnot_steps, any.missing = F, null.ok = F, min.chars = 1, unique = T)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var ifnot_steps}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_ifnot_steps")
  })
  #=============================================================================
  ifnot_steps_finder <- vector(mode = "logical", length = length(sc_final_steps)) %>% magrittr::set_names(rownames(sc))
  for (i in 1:length(sc_final_steps)) {
    ifnot_steps_finder[rownames(sc)[i]] <- ifnot_steps %>% magrittr::is_in(sc_final_steps[[i]]) %>% all_false() %>% magrittr::not()
  }
  # LEGACY: out <- sc[ifnot_steps_finder,]
  out <- sc %>% dplyr::filter(rownames(sc) %in% names(ifnot_steps_finder)[ifnot_steps_finder])
  #=============================================================================
  # Check output
  rlang::try_fetch({
    checkmate::assert_data_frame(out, types = "logical", any.missing = F, null.ok = F, min.cols = 1, col.names = "unique")
    checkmate::assert_character(rownames(out), pattern = "^cc[[:digit:]]+$")
  }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                             "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                           parent = cnd, class = "output")
  })
  #=============================================================================
  return(out)
}

#' Check IFNOT conditions
#'
#' Used in `create_scc()` and `check_causes_x_intv()`. Also see the documentation of `create_scc()` for a description of the process.
#'
#' @param re_sc A data.frame with colnames equal to the step IDs of the component causes and rownames of format ^cc[[:digit:]]+$. Contains only TRUE
#' or FALSE and no missings. It is a subset of the output of function `get_cause_combinations()` and cut to sets of component causes that fulfill the
#' outcome condition, i.e., are sufficient, and to those sufficient causes, which need to be re-checked for IFNOT conditions. `re_sc` has been
#' created by `get_sc_to_check_for_ifnot()`.
#' @param row An integer, indicating the row of `re_sc`. In `create_scc()`, the index of a for-loop is used for `row`.
#' @param sc_final_steps A named list with one element for each sufficient set of component causes. The list elements are character vectors
#' containing the final list of IDs of all steps included in the corresponding sufficient cause.
#' @param prc A processed steplist created with `process_steplist()`.
#' @param prc_split A list of length five containing the following elements: `non_start_steps`, `ifnot_steps`, causes, interventions, and end_steps,
#' of which the first two are used in this function:
#' * non_start_steps: A tibble with similar structur to `prc`. In fact, a subset of `prc`. If empty, the same tibble with 0 rows.
#' * ifnot_steps: A character vector. If empty, a character vector of length 0.
#' @param outc_list A data.frame with two columns, `sce` and `id`, which are both of type character. `sce` contains integers (as.character) and `id`
#' contains THEN statements of end steps. `outc_list` has been created by `transform_outc()`. It's the same in both `create_scc()` and
#' `check_ifnot()`.
#'
#' @returns A named list of length 4 with elements named as: `sc_status`, `order`, `incon`, and `incon_then`.
#' * sc_status: A single element of type character; either "always", "never", "depends", or "depends (potential order implausibilities)".
#' * order: NA, if `sc_status` equals "always" or "never". Otherwise a data.frame with two columns (`order` and `suff`), with `order` containing all
#'  relevant sequences of events as character and `suff` containing TRUE or FALSE indicating sufficiency for the corresponding sequence.
#' * incon: TRUE or FALSE. Only TRUE with `sc_status` equal to "depends (potential order implausibilities)".
#' * incon_then: NA, if `incon` equals FALSE. Otherwise a character vector containing THEN statements.
#'
#' @noRd
check_ifnot <- function(re_sc, row, sc_final_steps, prc, prc_split, outc_list) {
  # Check input
  rlang::try_fetch({
    checkmate::assert_tibble(prc, null.ok = F, ncols = 12, min.rows = 1, col.names = "unique")
    checkmate::assert_subset(colnames(prc), c("id_step","then_step","subject_step","does_step","object_step","where_step","if_step","if_list",
                                              "ifnot_step","ifnot_list","end_step","module_step"), empty.ok = F)
  }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var prc}",
                                             "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                           parent = cnd, class = "input_prc")
  })

  rlang::try_fetch({
      checkmate::assert_data_frame(re_sc, types = "logical", any.missing = F, null.ok = F, min.cols = 1, min.rows = 1,
                                   col.names = "unique", row.names = "unique")
      checkmate::assert_subset(colnames(re_sc), prc$id_step, empty.ok = F)
      checkmate::assert_character(rownames(re_sc), pattern = "^cc[[:digit:]]+$")
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var re_sc}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_re_sc")
  })

  rlang::try_fetch({
      checkmate::assert_integerish(row, lower = 1, any.missing = F, null.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var row}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_row")
  })

  rlang::try_fetch({
      checkmate::assert_list(sc_final_steps, any.missing = F, null.ok = F, min.len = 1, types = "character", names = "unique")
      checkmate::assert_character(names(sc_final_steps), pattern = "^cc[[:digit:]]+$")
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var sc_final_steps}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_sc_final_steps")
  })

  rlang::try_fetch({
      checkmate::assert_list(prc_split, any.missing = F, null.ok = F, len = 5, names = "unique")
      checkmate::assert_subset(names(prc_split), c("non_start_steps","causes","interventions","ifnot_steps","end_steps"), empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var prc_split}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_prc_split")
  })

  rlang::try_fetch({
      checkmate::assert_tibble(prc_split$non_start_steps, null.ok = F, ncols = 12, col.names = "unique")
      checkmate::assert_subset(colnames(prc_split$non_start_steps), colnames(prc), empty.ok = F)
      checkmate::assert_subset(prc_split$non_start_steps$id_step, prc$id_step)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var prc_split$non_start_steps}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_prc_split$non_start_steps")
  })

  rlang::try_fetch({
      checkmate::assert_character(prc_split$ifnot_steps, any.missing = F, null.ok = F, min.chars = 1, unique = T)
      if (length(prc_split$ifnot_steps) > 0) {
        checkmate::assert_subset(prc_split$ifnot_steps, prc$id_step, empty.ok = F)
      }
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var prc_split$ifnot_steps}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_prc_split$ifnot_steps")
  })

  # prc_split$end_steps is used for input checking of outc_list below
  rlang::try_fetch({
      checkmate::assert_character(prc_split$end_steps, any.missing = F, null.ok = F, min.len = 1, min.chars = 1, unique = T)
      checkmate::assert_subset(prc_split$end_steps, prc$id_step, empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var prc_split$end_steps}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_prc_split$end_steps")
  })

  rlang::try_fetch({
      checkmate::assert_data_frame(outc_list, types = "character", any.missing = F, null.ok = F, ncols = 2, min.rows = 1, col.names = "unique")
      checkmate::assert_subset(colnames(outc_list), c("sce","id"), empty.ok = F)
      checkmate::assert_integerish(outc_list$sce %>% as.numeric(), lower = 1, any.missing = F, null.ok = F)
      checkmate::assert_subset(outc_list$id, prc$then_step[prc$id_step %in% prc_split$end_steps], empty.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error {.var outc_list}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_outc_list")
  })
  #=============================================================================
  # Get the IFNOT conditions that appear in this sufficient cause
  relevant_ifnot_conditions <- prc %>% dplyr::filter(.data$id_step %in% sc_final_steps[[rownames(re_sc)[row]]]) %>%
    dplyr::filter(.data$id_step %in% prc_split[["ifnot_steps"]]) %>% magrittr::extract2("ifnot_step")

  # Check which IFNOT conditions are fulfilled within the steps of the sufficient cause
  fulfilled_ifnot_conditions <- rep(NA, length(relevant_ifnot_conditions)) %>% magrittr::set_names(relevant_ifnot_conditions)
  for (i in 1:length(fulfilled_ifnot_conditions)) {
    fulfilled_ifnot_conditions[i] <- is_fulfilled(if_list = sep_if_ifnot(names(fulfilled_ifnot_conditions)[i])[[1]],
                                                  current_list_then = sc_final_steps[[rownames(re_sc)[row]]] %>% sep_step() %>%
                                                    magrittr::extract2("then"))
  }
  ## End function when there are no relevant fulfilled IFNOT conditions
  if (fulfilled_ifnot_conditions %>% all_false()) {
    return(list(sc_status = "always", order = NA, incon = FALSE, incon_then = NA))
  }
  ## Summarize relevant fulfilled IFNOT conditions
  fulfilled_ifnot_conditions %<>% .[.] %>% names(.)

  # Get all steps that have fulfilled IFNOT conditions
  relevant_ifnot_steps <- prc %>% dplyr::filter(.data$id_step %in% sc_final_steps[[rownames(re_sc)[row]]]) %>%
    dplyr::filter(.data$ifnot_step %in% fulfilled_ifnot_conditions) %>%
    dplyr::select(dplyr::all_of(c("id_step" , "then_step", "if_step", "ifnot_step")))

  # Check for potential inconistencies
  potential_implausible_ordering <- FALSE
  if (relevant_ifnot_steps$then_step %>% magrittr::is_in(relevant_ifnot_steps$if_step) %>% all_false() %>% magrittr::not() |
      relevant_ifnot_steps$then_step %>% magrittr::is_in(relevant_ifnot_steps$ifnot_step) %>% all_false() %>% magrittr::not()) {
    potential_implausible_ordering <- TRUE
    potential_implausible_ordering_then <- c(relevant_ifnot_steps$then_step[relevant_ifnot_steps$then_step %in%
                                                                               relevant_ifnot_steps$if_step],
                                              relevant_ifnot_steps$then_step[relevant_ifnot_steps$then_step %in%
                                                                               relevant_ifnot_steps$ifnot_step]) %>%
                                            unique()
  }

  # Use THEN part for starting steps with IFNOT (after checking for implausibilities above because starting steps are not a problem in this regard)
  for (i in 1:nrow(relevant_ifnot_steps)) {
    if (is.na(relevant_ifnot_steps$if_step[i])) {
      relevant_ifnot_steps$if_step[i] <- relevant_ifnot_steps$then_step[i]
    }
  }

  # Get relevant conditions for permutation
  to_perm <- c(relevant_ifnot_steps$if_step,relevant_ifnot_steps$ifnot_step)

  # Get permutations, i.e., in which order the IF/IFNOT conditions appear
  order <- gtools::permutations(length(to_perm),length(to_perm), to_perm) %>% as.data.frame()

  # Get steps that need to be removed
  ## Check if IF happened before IFNOT
  to_remove <- matrix(rep(NA, nrow(order) * nrow(relevant_ifnot_steps)), nrow = nrow(order), ncol = nrow(relevant_ifnot_steps)) %>%
    as.data.frame() %>% magrittr::set_colnames(relevant_ifnot_steps$id_step)
  for (i in 1:nrow(to_remove)) {
    temp <- order[i,] %>% as.character()
    for (j in 1:ncol(to_remove)) {
      step_temp <- colnames(to_remove)[j] %>% sep_step()
      if (!is.na(step_temp[["if"]])) {
        if (which(temp == step_temp[["if"]]) > which(temp == step_temp[["ifnot"]])) {
          to_remove[i,j] <- TRUE
        } else {
          to_remove[i,j] <- FALSE
        }
      } else {
        if (which(temp == step_temp[["then"]]) > which(temp == step_temp[["ifnot"]])) {
          to_remove[i,j] <- TRUE
        } else {
          to_remove[i,j] <- FALSE
        }
      }
    }
  }
  ## Combine the steps that need to be removed based on IF/IFNOT order
  to_remove$list <- NA
  for (i in 1:nrow(to_remove)) {
    to_remove$list[i] <- colnames(to_remove)[c(to_remove[i,c(1:ncol(to_remove) - 1)] %>% t() %>% magrittr::extract(,1),FALSE)] %>%
      stringr::str_c(collapse = ";")
  }
  ## Add order
  to_remove$order <- NA
  for (i in 1:nrow(to_remove)) {
    to_remove$order[i] <- order[i,] %>% as.character() %>% stringr::str_c(collapse = "->")
  }
  ## Get unique sets of variables that need to be removed
  suff <- data.frame(remove = to_remove$list %>% unique(), suff = NA)

  # Check sufficiency when removing variables
  for (i in 1:nrow(suff)) {
    remove <- suff$remove[i] %>% stringr::str_split_1(";")
    ## Remove starting steps
    remove_cc <- re_sc[row,]
    if (remove %>% magrittr::is_in(colnames(remove_cc)) %>% all_false() %>% magrittr::not()) {
      for (j in 1:ncol(remove_cc)) {
        if (colnames(remove_cc)[j] %in% remove) {
          remove_cc[1,j] <- FALSE
        }
      }
    }
    ## Remove non-starting steps
    remove_non_start <- prc_split[["non_start_steps"]] %>% dplyr::filter(!(.data$id_step %in% remove))
    ## Check sufficiency
    suff$suff[i] <- is_sufficient(cc = remove_cc, row = 1, non_start_steps = remove_non_start, outc_list = outc_list)[[1]]
  }

  # Combine suff with to_remove
  to_remove %<>% dplyr::full_join(suff, by = c("list" = "remove"))

  if (to_remove$suff %>% all_true()) {
    return(list(sc_status = "always", order = NA, incon = FALSE, incon_then = NA))
  }
  if (to_remove$suff %>% all_false()) {
    return(list(sc_status = "never", order = NA, incon = FALSE, incon_then = NA))
  }
  if (to_remove$suff %>% all_false() %>% magrittr::not() & to_remove$suff %>% all_true() %>% magrittr::not()) {
    if (potential_implausible_ordering) {
      sc_status_temp <- "depends (potential order implausibilities)"
    } else {
      sc_status_temp <- "depends"
      potential_implausible_ordering_then <- NA
    }
    order <- to_remove %>% dplyr::select(dplyr::all_of(c("order", "suff")))
    #=============================================================================
    # Check output
    rlang::try_fetch({
        checkmate::assert_character(sc_status_temp, any.missing = F, null.ok = F, min.chars = 1, len = 1)
        checkmate::assert_choice(sc_status_temp, c("depends (potential order implausibilities)", "depends"))
      }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var sc_status_temp}",
                                                 "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                               parent = cnd, class = "output_sc_status_temp")
    })

    rlang::try_fetch({
        checkmate::assert_data_frame(order, types = c("character","logical"), any.missing = F, null.ok = F, ncols = 2, min.rows = 1,
                                     col.names = "unique")
        checkmate::assert_set_equal(colnames(order), c("order", "suff"), ordered = T)
        checkmate::assert_logical(order$suff, any.missing = F, null.ok = F)
        checkmate::assert_character(order$order, any.missing = F, null.ok = F, min.chars = 1, unique = T)
      }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var order}",
                                                 "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                               parent = cnd, class = "output_order")
    })

    rlang::try_fetch({
        checkmate::assert_logical(potential_implausible_ordering, any.missing = F, null.ok = F, len = 1)
      }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var potential_implausible_ordering}",
                                                 "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                               parent = cnd, class = "output_potential_implausible_ordering")
      })

    if (potential_implausible_ordering) {
      rlang::try_fetch({
          checkmate::assert_character(potential_implausible_ordering_then, null.ok = F, any.missing = F, min.chars = 1, min.len = 1)
        }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var potential_implausible_ordering_then}",
                                                   "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                                 parent = cnd, class = "output_potential_implausible_ordering_then")
      })
    } else {
      rlang::try_fetch({
          checkmate::assert_scalar_na(potential_implausible_ordering_then, null.ok = F)
        }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var potential_implausible_ordering_then}",
                                                   "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                                 parent = cnd, class = "output_potential_implausible_ordering_then")
      })
    }
    #=============================================================================
    return(list(sc_status = sc_status_temp, order = order,
                incon = potential_implausible_ordering, incon_then = potential_implausible_ordering_then))
  }
}

#' Add unknown causes to table of sufficient component cause sets
#'
#' Used in `create_scc()`.
#'
#' @param sc A data.frame with colnames equal to the step IDs of the component causes and rownames of format ^cc[[:digit:]]+$. Contains only TRUE or
#' FALSE and no missings. It is a subset of the output of function `get_cause_combinations()` and cut to sets of component causes that fulfill the
#' outcome condition, i.e., are sufficient.
#'
#' @returns An object similar to `sc` but extended by:
#' * one column to the right per sufficient cause with name "U[rownumber]" and all values equal to FALSE appart from row [rownumber]
#' * one additional column to the right with name "USC" and all values equal to FALSE for all sufficient causes
#' * one additional row with name "cc0" and all values equal to FALSE apart from column "USC", which is TRUE
#'
#' @noRd
unknown_sc <- function(sc) {
  # Check input
  rlang::try_fetch({
      checkmate::assert_data_frame(sc, types = "logical", any.missing = F, null.ok = F, min.cols = 1, min.rows = 1,
                                   col.names = "unique", row.names = "unique")
      checkmate::assert_character(rownames(sc), pattern = "^cc[[:digit:]]+$")
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var sc}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input_sc")
  })
  #=============================================================================
  # Create unknown component cause addition
  temp <- matrix(rep(FALSE, nrow(sc)^2), nrow = nrow(sc), ncol = nrow(sc))
  diag(temp) <- TRUE
  temp %<>% as.data.frame() %>% magrittr::set_colnames(paste0("U",c(1:nrow(sc))))

  # Combine with sc
  out <- cbind(sc,temp)

  # Add component cause of unknown sufficient cause
  out$USC <- FALSE

  # Add unknown sufficicent cause
  out <- rbind(out, c(rep(FALSE, ncol(out) - 1), TRUE))
  ## Unknown sufficient cause gets rowname "cc0"
  rownames(out)[nrow(out)] <- "cc0"
  #=============================================================================
  # Check output
  rlang::try_fetch({
      checkmate::assert_data_frame(out, types = "logical", any.missing = F, null.ok = F, ncols = ncol(sc) + nrow(sc) + 1, col.names = "unique",
                                   nrows = nrow(sc) + 1, row.names = "unique")
      checkmate::assert_set_equal(colnames(out), c(colnames(sc), paste0("U",c(1:nrow(sc))), "USC"), ordered = T)
      checkmate::assert_set_equal(rownames(out), c(rownames(sc), "cc0"), ordered = T)
      checkmate::assert_true(out["cc0","USC"])
      checkmate::assert_true(out["cc0", c(1:(ncol(sc) - 1))] %>% as.logical() %>% all_false())
    }, error = function(cnd) {cli::cli_abort(c("Output validation error",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output")
  })
  #=============================================================================
  return(out)
}
