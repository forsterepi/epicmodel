#' Checking `what` IDs
#'
#' Checking if the IDs in data.frame `what` of `steplist` are in the correct format and unique.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
check_what_ids <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  # Check what
  letter <- "a"
  pattern <- paste0("^",letter,"[:digit:]+$")
  if (nrow(steplist$what) > 0) {
    if (steplist$what$id_what %>% is.na() %>% all_true() %>% magrittr::not()) {
      if (steplist$what$id_what %>% stringr::str_detect(pattern) %>% all_true() %>% magrittr::not()) {
        wrong_ids <- steplist$what$id_what[steplist$what$id_what %>% stringr::str_detect(pattern) %>% magrittr::not()]
        cli::cli_abort(c("IDs in data.frame {.var what} of {.var steplist} must follow  format {.emph {pattern}},
                         i.e., start with lower case {.var {letter}} followed by digits, e.g., {letter}1,{letter}2, etc.!",
                         "i" = "Data.frame {.var what} contains {nrow(steplist$what)} element{?s}.",
                         "x" = "In total, {length(wrong_ids)} ID{?s} {?is/are} in the wrong format: {stringr::str_c(wrong_ids, collapse = ', ')}"),
                       class = "wrong_id_format")
      }

      if (steplist$what$id_what %>% duplicated() %>% all_false() %>% magrittr::not()) {
        dupli_ids <- steplist$what$id_what[steplist$what$id_what %>% duplicated()] %>% unique()
        cli::cli_abort(c("IDs in data.frame {.var what} of {.var steplist} must be unique!",
                         "i" = "Data.frame {.var what} contains {nrow(steplist$what)} element{?s}.",
                         "x" = "In total, {length(dupli_ids)} ID{?s} {?is/are} appearing multiple times:
                         {stringr::str_c(dupli_ids, collapse = ', ')}"),
                       class = "duplicated_ids")
      }
    }
  }
  cli::cli_alert_success("Checking WHAT IDs was successful.")
}

#' Checking `does` IDs
#'
#' Checking if the IDs in data.frame `does` of `steplist` are in the correct format and unique.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
check_does_ids <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  # Check does
  letter <- "d"
  pattern <- paste0("^",letter,"[:digit:]+$")
  if (nrow(steplist$does) > 0) {
    if (steplist$does$id_does %>% is.na() %>% all_true() %>% magrittr::not()) {
      if (steplist$does$id_does %>% stringr::str_detect(pattern) %>% all_true() %>% magrittr::not()) {
        wrong_ids <- steplist$does$id_does[steplist$does$id_does %>% stringr::str_detect(pattern) %>% magrittr::not()]
        cli::cli_abort(c("IDs in data.frame {.var does} of {.var steplist} must follow  format {.emph {pattern}},
                         i.e., start with lower case {.var {letter}} followed by digits, e.g., {letter}1,{letter}2, etc.!",
                         "i" = "Data.frame {.var does} contains {nrow(steplist$does)} element{?s}.",
                         "x" = "In total, {length(wrong_ids)} ID{?s} {?is/are} in the wrong format: {stringr::str_c(wrong_ids, collapse = ', ')}"),
                       class = "wrong_id_format")
      }

      if (steplist$does$id_does %>% duplicated() %>% all_false() %>% magrittr::not()) {
        dupli_ids <- steplist$does$id_does[steplist$does$id_does %>% duplicated()] %>% unique()
        cli::cli_abort(c("IDs in data.frame {.var does} of {.var steplist} must be unique!",
                         "i" = "Data.frame {.var does} contains {nrow(steplist$does)} element{?s}.",
                         "x" = "In total, {length(dupli_ids)} ID{?s} {?is/are} appearing multiple times:
                         {stringr::str_c(dupli_ids, collapse = ', ')}"),
                       class = "duplicated_ids")
      }
    }
  }
  cli::cli_alert_success("Checking DOES IDs was successful.")
}

#' Checking `where` IDs
#'
#' Checking if the IDs in data.frame `where` of `steplist` are in the correct format and unique.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
check_where_ids <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  # Check where
  letter <- "e"
  pattern <- paste0("^",letter,"[:digit:]+$")
  if (nrow(steplist$where) > 0) {
    if (steplist$where$id_where %>% is.na() %>% all_true() %>% magrittr::not()) {
      if (steplist$where$id_where %>% stringr::str_detect(pattern) %>% all_true() %>% magrittr::not()) {
        wrong_ids <- steplist$where$id_where[steplist$where$id_where %>% stringr::str_detect(pattern) %>% magrittr::not()]
        cli::cli_abort(c("IDs in data.frame {.var where} of {.var steplist} must follow  format {.emph {pattern}},
                         i.e., start with lower case {.var {letter}} followed by digits, e.g., {letter}1,{letter}2, etc.!",
                         "i" = "Data.frame {.var where} contains {nrow(steplist$where)} element{?s}.",
                         "x" = "In total, {length(wrong_ids)} ID{?s} {?is/are} in the wrong format: {stringr::str_c(wrong_ids, collapse = ', ')}"),
                       class = "wrong_id_format")
      }

      if (steplist$where$id_where %>% duplicated() %>% all_false() %>% magrittr::not()) {
        dupli_ids <- steplist$where$id_where[steplist$where$id_where %>% duplicated()] %>% unique()
        cli::cli_abort(c("IDs in data.frame {.var where} of {.var steplist} must be unique!",
                         "i" = "Data.frame {.var where} contains {nrow(steplist$where)} element{?s}.",
                         "x" = "In total, {length(dupli_ids)} ID{?s} {?is/are} appearing multiple times:
                         {stringr::str_c(dupli_ids, collapse = ', ')}"),
                       class = "duplicated_ids")
      }
    }
  }
  cli::cli_alert_success("Checking WHERE IDs was successful.")
}

#' Checking `module` IDs
#'
#' Checking if the IDs in data.frame `module` of `steplist` are in the correct format and unique.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
check_module_ids <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  # Check module
  letter <- "m"
  pattern <- paste0("^",letter,"[:digit:]+$")
  if (nrow(steplist$module) > 0) {
    if (steplist$module$id_module %>% is.na() %>% all_true() %>% magrittr::not()) {
      if (steplist$module$id_module %>% stringr::str_detect(pattern) %>% all_true() %>% magrittr::not()) {
        wrong_ids <- steplist$module$id_module[steplist$module$id_module %>% stringr::str_detect(pattern) %>% magrittr::not()]
        cli::cli_abort(c("IDs in data.frame {.var module} of {.var steplist} must follow  format {.emph {pattern}},
                         i.e., start with lower case {.var {letter}} followed by digits, e.g., {letter}1,{letter}2, etc.!",
                         "i" = "Data.frame {.var module} contains {nrow(steplist$module)} element{?s}.",
                         "x" = "In total, {length(wrong_ids)} ID{?s} {?is/are} in the wrong format: {stringr::str_c(wrong_ids, collapse = ', ')}"),
                       class = "wrong_id_format")
      }

      if (steplist$module$id_module %>% duplicated() %>% all_false() %>% magrittr::not()) {
        dupli_ids <- steplist$module$id_module[steplist$module$id_module %>% duplicated()] %>% unique()
        cli::cli_abort(c("IDs in data.frame {.var module} of {.var steplist} must be unique!",
                         "i" = "Data.frame {.var module} contains {nrow(steplist$module)} element{?s}.",
                         "x" = "In total, {length(dupli_ids)} ID{?s} {?is/are} appearing multiple times:
                         {stringr::str_c(dupli_ids, collapse = ', ')}"),
                       class = "duplicated_ids")
      }
    }
  }
  cli::cli_alert_success("Checking Module IDs was successful.")
}

#' Checking `icc` IDs
#'
#' Checking if the IDs in data.frame `icc` of `steplist` are in the correct format and unique.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
check_icc_ids <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  # Check icc
  letter <- "i"
  pattern <- paste0("^",letter,"[:digit:]+$")
  if (nrow(steplist$icc) > 0) {
    if (steplist$icc$id_icc %>% is.na() %>% all_true() %>% magrittr::not()) {
      if (steplist$icc$id_icc %>% stringr::str_detect(pattern) %>% all_true() %>% magrittr::not()) {
        wrong_ids <- steplist$icc$id_icc[steplist$icc$id_icc %>% stringr::str_detect(pattern) %>% magrittr::not()]
        cli::cli_abort(c("IDs in data.frame {.var icc} of {.var steplist} must follow  format {.emph {pattern}},
                         i.e., start with lower case {.var {letter}} followed by digits, e.g., {letter}1,{letter}2, etc.!",
                         "i" = "Data.frame {.var icc} contains {nrow(steplist$icc)} element{?s}.",
                         "x" = "In total, {length(wrong_ids)} ID{?s} {?is/are} in the wrong format: {stringr::str_c(wrong_ids, collapse = ', ')}"),
                       class = "wrong_id_format")
      }

      if (steplist$icc$id_icc %>% duplicated() %>% all_false() %>% magrittr::not()) {
        dupli_ids <- steplist$icc$id_icc[steplist$icc$id_icc %>% duplicated()] %>% unique()
        cli::cli_abort(c("IDs in data.frame {.var icc} of {.var steplist} must be unique!",
                         "i" = "Data.frame {.var icc} contains {nrow(steplist$icc)} element{?s}.",
                         "x" = "In total, {length(dupli_ids)} ID{?s} {?is/are} appearing multiple times:
                         {stringr::str_c(dupli_ids, collapse = ', ')}"),
                       class = "duplicated_ids")
      }
    }
  }
  cli::cli_alert_success("Checking ICC IDs was successful.")
}

#' Checking `what` keywords
#'
#' Checking if the keywords in data.frame `what` of `steplist` are unique.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
check_what_keys <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  # Check what keys
  if (nrow(steplist$what) > 0) {
    if (steplist$what$key_what %>% duplicated() %>% all_false() %>% magrittr::not()) {
      dupli_keys <- steplist$what$key_what[steplist$what$key_what %>% duplicated()] %>% unique()
      cli::cli_warn(c("Keywords in data.frame {.var what} of {.var steplist} are not unique!",
                      "i" = "Data.frame {.var what} contains {nrow(steplist$what)} element{?s}.",
                      "i" = "In total, {length(dupli_keys)} keyword{?s} {?is/are} appearing multiple times:
                      {stringr::str_c(dupli_keys, collapse = ', ')}"),
                     class = "duplicated_keys")
    }
  }
  cli::cli_alert_success("Checking WHAT keywords was successful.")
}

#' Checking `does` keywords
#'
#' Checking if the keywords in data.frame `does` of `steplist` are unique.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
check_does_keys <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  # Check does keys
  if (nrow(steplist$does) > 0) {
    if (steplist$does$key_does %>% duplicated() %>% all_false() %>% magrittr::not()) {
      dupli_keys <- steplist$does$key_does[steplist$does$key_does %>% duplicated()] %>% unique()
      cli::cli_warn(c("Keywords in data.frame {.var does} of {.var steplist} are not unique!",
                      "i" = "Data.frame {.var does} contains {nrow(steplist$does)} element{?s}.",
                      "i" = "In total, {length(dupli_keys)} keyword{?s} {?is/are} appearing multiple times:
                      {stringr::str_c(dupli_keys, collapse = ', ')}"),
                    class = "duplicated_keys")
    }
  }
  cli::cli_alert_success("Checking DOES keywords was successful.")
}

#' Checking `where` keywords
#'
#' Checking if the keywords in data.frame `where` of `steplist` are unique.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
check_where_keys <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  # Check where keys
  if (nrow(steplist$where) > 0) {
    if (steplist$where$key_where %>% duplicated() %>% all_false() %>% magrittr::not()) {
      dupli_keys <- steplist$where$key_where[steplist$where$key_where %>% duplicated()] %>% unique()
      cli::cli_warn(c("Keywords in data.frame {.var where} of {.var steplist} are not unique!",
                      "i" = "Data.frame {.var where} contains {nrow(steplist$where)} element{?s}.",
                      "i" = "In total, {length(dupli_keys)} keyword{?s} {?is/are} appearing multiple times:
                      {stringr::str_c(dupli_keys, collapse = ', ')}"),
                    class = "duplicated_keys")
    }
  }
  cli::cli_alert_success("Checking WHERE keywords was successful.")
}

#' Checking `module` keywords
#'
#' Checking if the keywords in data.frame `module` of `steplist` are unique.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
check_module_keys <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  # Check module keys
  if (nrow(steplist$module) > 0) {
    if (steplist$module$key_module %>% duplicated() %>% all_false() %>% magrittr::not()) {
      dupli_keys <- steplist$module$key_module[steplist$module$key_module %>% duplicated()] %>% unique()
      cli::cli_warn(c("Keywords in data.frame {.var module} of {.var steplist} are not unique!",
                      "i" = "Data.frame {.var module} contains {nrow(steplist$module)} element{?s}.",
                      "i" = "In total, {length(dupli_keys)} keyword{?s} {?is/are} appearing multiple times:
                      {stringr::str_c(dupli_keys, collapse = ', ')}"),
                    class = "duplicated_keys")
    }
  }
  cli::cli_alert_success("Checking Module keywords was successful.")
}

#' Are modules used?
#'
#' Check if modules have been used in an `epicmodel_steplist` object.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
are_modules_used <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })
  # No use, if nrow(steplist$module) is 0
  if (nrow(steplist$module) == 0) {
    modules_used <- FALSE
  } else {
    # No use, if only empty strings or NA as strings are the keys
    if (steplist$module$key_module %>% magrittr::is_in(c("NA","na",""," ")) %>% all_true()) {
      modules_used <- FALSE
    } else {
      # No use, if module keys contain only actual NAs
      if (steplist$module$key_module %>% is.na() %>% all_true()) {
        modules_used <- FALSE
      } else {
      # Otherwise yes
      modules_used <- TRUE
      }
    }
  }

  return(modules_used)
}

#' Check modules
#'
#' Checks if all modules in `steplist$module` have been used, if all modules in `steplist$step$module_step` have been specified in
#' `steplist$module` and if all steps have a module defined in case the user wants to use them.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
check_modules_in_steps <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  # Only continue if modules are actually used
  if (are_modules_used(steplist)) {
    ## Unspecified modules
    if (steplist$step$module_step %>% magrittr::is_in(c(steplist$module$id_module,"")) %>% all_true() %>% magrittr::not()) {
      unspecified_modules <- steplist$step$module_step[steplist$step$module_step %>% magrittr::is_in(c(steplist$module$id_module,"")) %>%
                                                    magrittr::not()] %>% unique()
      cli::cli_abort(c("All modules in data.frame {.var step} must be specified in data.frame {.var modules}!",
                     "i" = "Data.frame {.var step} contains {nrow(steplist$step)} element{?s}.",
                     "x" = "In total, {length(unspecified_modules)} module{?s} used in data.frame {.var step} {?has/have} not been
                     specified in data.frame {.var module}: {stringr::str_c(unspecified_modules, collapse = ', ')}"),
                     class = "unspecified_modules")
    }
    ## Unused modules
    if (steplist$module$id_module %>% magrittr::is_in(steplist$step$module_step) %>% all_true() %>% magrittr::not()) {
      unused_modules <- steplist$module$id_module[steplist$module$id_module %>% magrittr::is_in(steplist$step$module_step) %>%
                                                    magrittr::not()] %>% unique()
      cli::cli_warn(c("Not all modules have been used in data.frame {.var step}!",
                    "i" = "Data.frame {.var module} contains {nrow(steplist$module)} element{?s}.",
                    "i" = "In total, {length(unused_modules)} module{?s} {?is/are} not being used in data.frame {.var step}:
                    {stringr::str_c(unused_modules, collapse = ', ')}"),
                    class = "unused_modules")
    }
    ## Inconsistent module use in steplist$step
    if (steplist$step$module_step %>% magrittr::is_in("") %>% all_false() %>% magrittr::not()) {
      number_empty_modules <- steplist$step$module_step %>% magrittr::is_in("") %>% sum()
      cli::cli_abort(c("Either all steps or none must have a module specified, i.e., a value in {.var module_step} of data.frame {.var step}!",
                       "i" = "Data.frame {.var step} contains {nrow(steplist$step)} element{?s}.",
                       "x" = "For {number_empty_modules} step{?s}, no module has been specified.",
                       "i" = "Call {.code steplist <- remove_all_modules(steplist)}, if you want to continue without modules."),
                     class = "empty_modules_in_step")
    }
  }

  cli::cli_alert_success("Checking Modules was successful.")
}

#' Check ICC entries
#'
#' Checks if steps used to define incompatible component causes (ICC) are in `steplist$step`.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
check_steps_in_icc <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  # Get IDs of steps used in steplist$icc
  icc_ids <- c(steplist$icc$id1, steplist$icc$id2) %>% unique()

  # Check if all used IDs are in steplist$step
  if (length(icc_ids) > 0) {
    if (icc_ids %>% magrittr::is_in(steplist$step$id_step) %>% all_true() %>% magrittr::not()) {
      not_available <- icc_ids[icc_ids %>% magrittr::is_in(steplist$step$id_step) %>% magrittr::not()]
      cli::cli_abort(c("All IDs in data.frame {.var icc}, i.e. in variables {.var id1} and {.var id2}, must be in {.var id_step} of
                       data.frame {.var step}!",
                       "i" = "Data.frame {.var icc} contains {nrow(steplist$icc)} element{?s} with two step IDs each.",
                       "x" = "In total, {length(not_available)} ID{?s} {?is/are} not in data.frame {.var step}:
                       {stringr::str_c(not_available, collapse = ', ')}",
                       "i" = "If only {.var NA} is not in data.frame {.var step}, use {.code steplist <- remove_na(steplist)}."),
                     class = "unspecified_steps_in_icc")
    }
  }

  cli::cli_alert_success("Checking ICC entries was successful.")
}

#' Check WHAT segments
#'
#' Checks if all WHAT segments in `steplist$what` have been used in steps and if all used WHAT segments have been specified in `steplist$what`.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
check_what_in_steps <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  # Get WHAT segments used in steplist$step
  prc <- process_steplist(steplist)
  used_what <- c(prc$subject_step,prc$object_step) %>% .[!is.na(.)]
  used_what <- used_what[used_what %>% stringr::str_detect("^a[:digit:]+$")]

  if (length(used_what) > 0) {
    ## Unspecified WHAT segments
    if (used_what %>% magrittr::is_in(steplist$what$id_what) %>% all_true() %>% magrittr::not()) {
      unspecified_segments <- used_what[used_what %>% magrittr::is_in(steplist$what$id_what) %>% magrittr::not()] %>% unique()
      cli::cli_abort(c("All WHAT segments in data.frame {.var step} must be specified in data.frame {.var what}!",
                       "i" = "Data.frame {.var step} contains {nrow(steplist$step)} element{?s}.",
                       "x" = "In total, {length(unspecified_segments)} WHAT segment{?s} in data.frame {.var step} {?has/have} not
                       been specified in data.frame {.var what}: {stringr::str_c(unspecified_segments, collapse = ', ')}"),
                     class = "unspecified_segments")
    }
    ## Unused WHAT segments
    if (steplist$what$id_what %>% .[!is.na(.)] %>% magrittr::is_in(used_what) %>% all_true() %>% magrittr::not()) {
      unused_segments <- steplist$what$id_what[steplist$what$id_what %>% magrittr::is_in(used_what) %>% magrittr::not()] %>% unique()
      cli::cli_warn(c("Not all WHAT segments have been used in data.frame {.var step}!",
                      "i" = "Data.frame {.var what} contains {nrow(steplist$what)} element{?s}.",
                      "i" = "In total, {length(unused_segments)} WHAT segment{?s} {?is/are} not being used in data.frame {.var step}:
                      {stringr::str_c(unused_segments, collapse = ', ')}"),
                    class = "unused_segments")
    }
  }
  cli::cli_alert_success("Checking WHAT segments was successful.")
}

#' Check DOES segments
#'
#' Checks if all DOES segments in `steplist$does` have been used in steps and if all used DOES segments have been specified in `steplist$does`.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
check_does_in_steps <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  # Get DOES segments used in steplist$step
  prc <- process_steplist(steplist)
  used_does <- prc$does_step %>% .[!is.na(.)]

  if (length(used_does) > 0) {
    ## Unspecified DOES segments
    if (used_does %>% magrittr::is_in(steplist$does$id_does) %>% all_true() %>% magrittr::not()) {
      unspecified_segments <- used_does[used_does %>% magrittr::is_in(steplist$does$id_does) %>% magrittr::not()] %>% unique()
      cli::cli_abort(c("All DOES segments in data.frame {.var step} must be specified in data.frame {.var does}!",
                       "i" = "Data.frame {.var step} contains {nrow(steplist$step)} element{?s}.",
                       "x" = "In total, {length(unspecified_segments)} DOES segment{?s} used in data.frame {.var step} {?has/have} not
                       been specified in data.frame {.var does}: {stringr::str_c(unspecified_segments, collapse = ', ')}"),
                     class = "unspecified_segments")
    }
    ## Unused DOES segments
    if (steplist$does$id_does %>% .[!is.na(.)] %>% magrittr::is_in(used_does) %>% all_true() %>% magrittr::not()) {
      unused_segments <- steplist$does$id_does[steplist$does$id_does %>% magrittr::is_in(used_does) %>% magrittr::not()] %>% unique()
      cli::cli_warn(c("Not all DOES segments have been used in data.frame {.var step}!",
                      "i" = "Data.frame {.var does} contains {nrow(steplist$does)} element{?s}.",
                      "i" = "In total, {length(unused_segments)} DOES segment{?s} {?is/are} not being used in data.frame {.var step}:
                      {stringr::str_c(unused_segments, collapse = ', ')}"),
                    class = "unused_segments")
    }
  }
  cli::cli_alert_success("Checking DOES segments was successful.")
}

#' Check WHERE segments
#'
#' Checks if all WHERE segments in `steplist$where` have been used in steps and if all used WHERE segments have been specified in `steplist$where`.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
check_where_in_steps <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  # Get WHERE segments used in steplist$step
  prc <- process_steplist(steplist)
  used_where <- prc$where_step %>% .[!is.na(.)]

  if (length(used_where) > 0) {
    ## Unspecified WHERE segments
    if (used_where %>% magrittr::is_in(steplist$where$id_where) %>% all_true() %>% magrittr::not()) {
      unspecified_segments <- used_where[used_where %>% magrittr::is_in(steplist$where$id_where) %>% magrittr::not()] %>% unique()
      cli::cli_abort(c("All WHERE segments in data.frame {.var step} must be specified in data.frame {.var where}!",
                       "i" = "Data.frame {.var step} contains {nrow(steplist$step)} element{?s}.",
                       "x" = "In total, {length(unspecified_segments)} WHERE segment{?s} used in data.frame {.var step} {?has/have} not
                       been specified in data.frame {.var where}: {stringr::str_c(unspecified_segments, collapse = ', ')}"),
                     class = "unspecified_segments")
    }
    ## Unused WHERE segments
    if (steplist$where$id_where %>% .[!is.na(.)] %>% magrittr::is_in(used_where) %>% all_true() %>% magrittr::not()) {
      unused_segments <- steplist$where$id_where[steplist$where$id_where %>% magrittr::is_in(used_where) %>% magrittr::not()] %>% unique()
      cli::cli_warn(c("Not all WHERE segments have been used in data.frame {.var step}!",
                      "i" = "Data.frame {.var where} contains {nrow(steplist$where)} element{?s}.",
                      "i" = "In total, {length(unused_segments)} WHERE segment{?s} {?is/are} not being used in data.frame {.var step}:
                      {stringr::str_c(unused_segments, collapse = ', ')}"),
                    class = "unused_segments")
    }
  }
  cli::cli_alert_success("Checking WHERE segments was successful.")
}

#' Check references
#'
#' Checks if all steps in data.frame `step` have references specified.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
check_refs <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  # Get references
  refs <- steplist$step$ref_step

  # Check how many are invalid
  no_refs <- refs %>% magrittr::is_in(c("NA","na"," ","",NA)) %>% sum()

  if (no_refs > 0) {
    cli::cli_warn(c("For some steps no references have been provided!",
                    "i" = "In total, {no_refs} step{?s} {?has/have} no references."), class = "no_refs")
  }

  cli::cli_alert_success("Checking references was successful.")
}

#' Check start and end steps
#'
#' Checks if there are Start-steps with `end_step = 1`.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
check_start_end_steps <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  # Get start steps with end_step = 1
  start_end <- steplist$step %>% dplyr::filter(.data$desc_step %>% stringr::str_detect("^Start:")) %>% dplyr::filter(.data$end_step == "1")

  if (nrow(start_end) > 0) {
    wrong_ids <- start_end$id_step %>% stringr::str_c(collapse = ', ')
    cli::cli_abort(c("Starting steps, i.e., component causes or interventions, i.e., steps beginning with 'Start:', must not have
                     {.var end_step == 1}!",
                     "i" = "There {?is/are} {nrow(start_end)} starting step{?s} with {.var end_step == 1}.",
                     "i" = "Namely: {wrong_ids}"),
                   class = "start_end_error")
  }

  # Have component cause been specified, i.e., start steps but no interventions
  prc <- steplist %>% process_steplist()
  if_vars <- prc$if_list %>% purrr::map_dfr(as.data.frame) %>% magrittr::extract2("id") %>% unique() %>% .[!is.na(.)]
  ifnot_vars <- prc$ifnot_list %>% purrr::map_dfr(as.data.frame) %>% magrittr::extract2("id") %>% unique() %>% .[!is.na(.)]
  interventions <- prc %>% dplyr::filter(is.na(.data$if_step)) %>% dplyr::filter((.data$then_step %in% ifnot_vars) & !(.data$then_step %in% if_vars))
  causes <- prc %>% dplyr::filter(is.na(.data$if_step)) %>% dplyr::filter(!(.data$id_step %in% interventions$id_step))

  if (nrow(causes) == 0) {
    cli::cli_abort("There are no component causes, i.e., start steps that are not interventions, in the steplist!",
                   class = "no_cc")
  }

  cli::cli_alert_success("Checking start/end steps was successful.")
}

#' Check THEN statements
#'
#' Checks if there are steps that can chain together with all IF/IFNOT conditions, i.e., if there are steps that have the corresponding
#' statement as THEN part. Also checks, if there are duplicated THEN statements and if they have different values for `end_step`. Also checkes if
#' there are steps who's THEN statement is not in THEN. This might cause problems when merging the THEN description to the steps vie `steplist$then`.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
check_then_use <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  # Process steplist
  prc <- process_steplist(steplist)

  if (nrow(prc) > 0) {
    if (prc$id_step %>% is.na() %>% all_true() %>% magrittr::not()) {
      ## Identify THEN subsets
      then_in_if <- prc$if_list %>% purrr::map_dfr(as.data.frame) %>% dplyr::filter(!is.na(.data$id)) %>% magrittr::extract2("id")
      then_in_ifnot <- prc$ifnot_list %>% purrr::map_dfr(as.data.frame) %>% dplyr::filter(!is.na(.data$id)) %>% magrittr::extract2("id")
      then_in_if_ifnot <- c(then_in_if,then_in_ifnot) %>% unique()
      then_not_end <- prc %>% dplyr::filter(.data$end_step != "1") %>% magrittr::extract2("then_step")
      then_end <- prc %>% dplyr::filter(.data$end_step == "1") %>% magrittr::extract2("then_step")
      ## THEN used multiple times with varying end_step
      if (then_end %>% magrittr::is_in(then_not_end) %>% all_false() %>% magrittr::not()) {
        dupli_end <- then_end[then_end %>% magrittr::is_in(then_not_end)]
        cli::cli_abort(c("Steps with identical THEN statements must not have both {.emph end_step = 0} and {.emph end_step = 1}!",
                         "x" = "In total, {length(dupli_end)} THEN statement{?s} {?is/are} available multiple times with different
                         {.var end_step} values: {stringr::str_c(dupli_end, collapse = ', ')}"),
                       class = "dupli_then_with_varying_end_step")
      }
      ## No steps available with THEN that appear in IF/IFNOT
      if (then_in_if_ifnot %>% magrittr::is_in(then_not_end) %>% all_true() %>% magrittr::not()) {
       unavailable_if_ifnot <- then_in_if_ifnot[then_in_if_ifnot %>% magrittr::is_in(then_not_end) %>% magrittr::not()]
       cli::cli_abort(c("All THEN statements used in IF/IFNOT conditions must be available for chaining!",
                      "i" = "There must be a step with this statement as its THEN part. This step must not be defined as {.emph end_step = 1}.",
                      "x" = "In total, {length(unavailable_if_ifnot)} THEN statement{?s} used in IF/IFNOT conditions {?is/are} not available
                      as step{?s}: {stringr::str_c(unavailable_if_ifnot, collapse = ', ')}"),
                      class = "unavailable_if_ifnot")
      }
      ## Steps without THEN statement in steplist$then
      if (prc$then_step %>% magrittr::is_in(steplist$then$id_then) %>% all_true() %>% magrittr::not()) {
        unavailable_then <- prc$then_step[prc$then_step %>% magrittr::is_in(steplist$then$id_then) %>% magrittr::not()]
        cli::cli_abort(c("For all steps, their THEN statements must be part of data.frame {.var then} of {.var steplist}!",
                         "x" = "In total, {length(unavailable_then)} step{?s} {?has/have} no corresponding entry in data.frame {.var then}:
                         {stringr::str_c(unavailable_then, collapse = ', ')}"),
                       class = "unavailable_then")
      }
      ## Duplicated THEN statements
      if (prc$then_step %>% duplicated() %>% all_false() %>% magrittr::not()) {
        then_duplicates <- prc$then_step[prc$then_step %>% duplicated()]
        cli::cli_warn(c("There are steps with duplicated THEN statements!",
                        "i" = "In total, {length(then_duplicates)} step{?s} {?has/have} THEN statements that already appeared in other steps:
                        {stringr::str_c(then_duplicates, collapse = ', ')}"),
                      class = "then_duplicates")
      }
    }
  }

  cli::cli_alert_success("Checking THEN statements was successful.")
}

#' Check equality of IF/IFNOT/THEN
#'
#' Checks if there are steps that same the same IF/IFNOT conditions or if the THEN statement appears in its own IF/IFNOT condition.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
check_then_if_ifnot <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  # Process steplist
  prc <- process_steplist(steplist)

  if (nrow(prc) > 0) {
    if (prc$id_step %>% is.na() %>% all_true() %>% magrittr::not()) {
      ## Create empty container
      if_ifnot_equality <- rep(FALSE, nrow(prc))
      then_in_if <- rep(FALSE, nrow(prc))
      then_in_ifnot <- rep(FALSE, nrow(prc))
      ## Loop over every step
      for (i in 1:nrow(prc)) {
        ## Extract information
        if_temp <- prc$if_step[i]
        ifnot_temp <- prc$ifnot_step[i]
        if_then_temp <- prc$if_list[[i]]$id
        ifnot_then_temp <- prc$ifnot_list[[i]]$id
        then_temp <- prc$then_step[i]
        ## Check if IF and IFNOT are the same
        if (!is.na(if_temp) & !is.na(ifnot_temp)) {
          if_ifnot_equality[i] <- if_temp == ifnot_temp
        }
        ## Check if THEN is in IF
        if (!is.na(if_temp)) {
          then_in_if[i] <- then_temp %in% if_then_temp
        }
        ## Check if THEN is in IFNOT
        if (!is.na(ifnot_temp)) {
          then_in_ifnot[i] <- then_temp %in% ifnot_then_temp
        }
      }
      ## Report IF IFNOT equality
      if (if_ifnot_equality %>% all_false() %>% magrittr::not()) {
        if_ifnot_equality_steps <- prc$id_step[if_ifnot_equality] %>% stringr::str_c(collapse = ', ')
        cli::cli_abort(c("Some steps have identical IF and IFNOT conditions!",
                        "x" = "Applies to the following steps: {if_ifnot_equality_steps}"),
                      class = "if_ifnot_equality")
      }
      ## Report THEN IF equality
      if (then_in_if %>% all_false() %>% magrittr::not()) {
        then_in_if_steps <- prc$id_step[then_in_if] %>% stringr::str_c(collapse = ', ')
        cli::cli_abort(c("For some steps the THEN statement appears in its own IF condition!",
                         "x" = "Applies to the following steps: {then_in_if_steps}"),
                       class = "then_in_if")
      }
      ## Report THEN IFNOT equality
      if (then_in_ifnot %>% all_false() %>% magrittr::not()) {
        then_in_ifnot_steps <- prc$id_step[then_in_ifnot] %>% stringr::str_c(collapse = ', ')
        cli::cli_abort(c("For some steps the THEN statement appears in its own IFNOT condition!",
                         "x" = "Applies to the following steps: {then_in_ifnot_steps}"),
                       class = "then_in_ifnot")
      }
    }
  }

  cli::cli_alert_success("Checking THEN/IF/IFNOT equality was successful.")
}

#' Check outcome definition
#'
#' Checks if there is avalid outcome definition.
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @noRd
check_outc <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  # Check emptyness
  if (nrow(steplist$outc) == 0) {
    outc_empty <- TRUE
  } else {
    if (steplist$outc$id_outc %>% magrittr::is_in(c("NA","na",""," ")) %>% all_false() %>% magrittr::not()) {
      outc_empty <- TRUE
    } else {
      if (steplist$outc$id_outc %>% is.na() %>% all_false() %>% magrittr::not()) {
        outc_empty <- TRUE
      } else {
        outc_empty <- FALSE
      }
  }}
  if (outc_empty) {cli::cli_abort("Data.frame {.var outc} does contain invalid outcome definitions or is empty!", class = "outc_empty")}

  # Get THEN statements used in outcome definitions
  outc_lines <- nrow(steplist$outc)
  outc_list <- vector(mode = "list", length = outc_lines)
  for (i in 1:outc_lines) {
    temp <- steplist$outc$id_outc[i] %>% stringr::str_split("\\+") %>% magrittr::extract2(1)
    outc_list[[i]] <- temp
  }
  used_then <- outc_list %>% purrr::map_dfr(as.data.frame) %>% magrittr::extract2(1) %>% unique()

  # Get THEN statements of end steps
  end_then <- steplist$step %>% dplyr::filter(.data$end_step == "1") %>% magrittr::extract2("id_step") %>% sep_step() %>% magrittr::extract2("then")

  # Compare used_then to end_then
  if (length(used_then) > 0) {
    if (used_then %>% magrittr::is_in(end_then) %>% all_true() %>% magrittr::not()) {
      not_available <- used_then[used_then %>% magrittr::is_in(end_then) %>% magrittr::not()]
      cli::cli_abort(c("All steps used in data.frame {.var outc} must be in data.frame {.var step} and must be defined as {.var end_step = 1}!",
                       "x" = "In total, {length(not_available)} step{?s} {?is/are} not in data.frame `step`:
                       {stringr::str_c(not_available, collapse = ', ')}"),
                     class = "unspecified_steps_in_outc")
    }
  }

  if (length(end_then) > 0) {
    if (end_then %>% magrittr::is_in(used_then) %>% all_true() %>% magrittr::not()) {
      not_used <- end_then[end_then %>% magrittr::is_in(used_then) %>% magrittr::not()]
      cli::cli_warn(c("Some steps that are defined as {.var end_step = 1} are not used in outcome definitions, i.e., in data.frame {.var outc}!",
                       "x" = "In total, {length(not_used)} end step{?s} {?is/are} not in data.frame `outc` (with THEN):
                      {stringr::str_c(not_used, collapse = ', ')}"),
                     class = "unused_steps_in_outc")
    }
  }

  # Subsets
  if (nrow(steplist$outc) > 1) {
    outc_subsets <- FALSE
    outc_parts <- steplist$outc$id_outc %>% stringr::str_split("\\+")

    for (i in 1:length(outc_parts)) {
      for (j in 1:length(outc_parts)) {
        all_of_mine_in_yours <- outc_parts[[i]] %>% magrittr::is_in(outc_parts[[j]]) %>% all_true()
        all_of_yours_in_mine <- outc_parts[[j]] %>% magrittr::is_in(outc_parts[[i]]) %>% all_true()
        if (all_of_mine_in_yours & !(all_of_yours_in_mine)) {
          outc_subsets <- TRUE
        }}}

    if (outc_subsets) {
      cli::cli_warn("Some outcome definitions are subsets of each other!",
                    class = "outc_subsets")
    }
  }

  cli::cli_alert_success("Checking outcome definitions was successful.")
}

#' Check `epicmodel_steplist` class objects
#'
#' Check if `epicmodel_steplist` class objects fulfill the conditions for being inputed in [create_scc()].
#'
#' @details
#' The following checks are conducted:
#'
#' ## Errors
#' * Correct ID format in WHAT segments
#' * No duplicated IDs in WHAT segments
#' * Correct ID format in DOES segments
#' * No duplicated IDs in DOES segments
#' * Correct ID format in WHERE segments
#' * No duplicated IDs in WHERE segments
#' * Correct ID format in Modules
#' * No duplicated IDs in Modules
#' * Correct ID format in ICC
#' * No duplicated IDs in ICC
#' * All WHAT segments used in data.frame `step` must be listed in data.frame `what`
#' * All DOES segments used in data.frame `step` must be listed in data.frame `does`
#' * All WHERE segments used in data.frame `step` must be listed in data.frame `where`
#' * All modules used in data.frame `step` must be listed in data.frame `modules`
#' * Either all steps or no steps have modules specified in data.frame `step`
#' * All step IDs used in ICC definition must be specified in data.frame `step`
#' * Starting steps, i.e., steps without IF condition, must not have `end_step == 1` in data.frame `step`
#' * A steplist must contain component causes
#' * In case there are two steps with identical THEN statements, they cannot have both `end_step == 1` and `end_step == 0` in data.frame `step`
#' * THEN statements used in IF/IFNOT conditions must be available for chaining, i.e., there must be a step with this statement as its THEN part
#'   and this step must not be defined as end step
#' * For all steps, their THEN statement must be available in data.frame `then`
#' * A step must not have identical IF and IFNOT conditions
#' * A step’s THEN statement must not be part of its own IF/IFNOT condition
#' * All steps used in the outcome definition must be in data.frame `step` with `end_step == 1`
#'
#' ## Warnings
#' * No duplicated keywords in WHAT segments
#' * No duplicated keywords in DOES segments
#' * No duplicated keywords in WHERE segments
#' * No duplicated keywords in Modules
#' * All WHAT segments in data.frame `what` should be used in data.frame `step`
#' * All DOES segments in data.frame `does` should be used in data.frame `step`
#' * All WHERE segments in data.frame `where` should be used in data.frame `step`
#' * All modules in data.frame `modules` should be used in data.frame `step`
#' * All steps should have references
#' * There should not be any steps with identical THEN statements
#' * All steps with `end_step == 1`in data.frame `step` should be used in the outcome definition
#' * Outcome definitions should not be contained in each other, e.g., for outcome definition `(A and B) or (A and B and C)`, `A and B` is contained,
#'   i.e., a subset of, `A and B and C`, which makes `A and B and C` redundant
#'
#' @param steplist An object of class `epicmodel_steplist`.
#'
#' @returns Prints information about successful and unsuccessful checks in the console. Returns the input steplist. If checks were successful,
#' returns a steplist of class `epicmodel_steplist_checked` that can be used in building SCC models.
#'
#' @export
#'
#' @examples
#' steplist_checked <- check_steplist(steplist_rain)
check_steplist <- function(steplist) {
  # Check steplist
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })

  cli::cli_h1("Checking {.emph epicmodel_steplist} {as.character(substitute(steplist))}")

  # Uncheck
  steplist %<>% uncheck_steplist()

  # Run all checks
  rlang::try_fetch(check_what_ids(steplist),
                   error = function(e_cnd) {
                     cli::cli_inform(c("x" = "Checking WHAT IDs failed!"), parent = e_cnd)
                   })

  rlang::try_fetch(check_does_ids(steplist),
                   error = function(e_cnd) {
                     cli::cli_inform(c("x" = "Checking DOES IDs failed!"), parent = e_cnd)
                   })

  rlang::try_fetch(check_where_ids(steplist),
                   error = function(e_cnd) {
                     cli::cli_inform(c("x" = "Checking WHERE IDs failed!"), parent = e_cnd)
                   })

  rlang::try_fetch(check_module_ids(steplist),
                   error = function(e_cnd) {
                     cli::cli_inform(c("x" = "Checking Module IDs failed!"), parent = e_cnd)
                   })

  rlang::try_fetch(check_icc_ids(steplist),
                   error = function(e_cnd) {
                     cli::cli_inform(c("x" = "Checking ICC IDs failed!"), parent = e_cnd)
                   })

  rlang::try_fetch(check_what_keys(steplist),
                   error = function(e_cnd) {
                     cli::cli_inform(c("x" = "Checking WHAT keywords failed!"), parent = e_cnd)
                   },
                   warning = function(w_cnd) {
                     cli::cli_inform(c("!" = "Checking WHAT keywords resulted in warnings!"), parent = w_cnd)
                   })

  rlang::try_fetch(check_does_keys(steplist),
                   error = function(e_cnd) {
                     cli::cli_inform(c("x" = "Checking DOES keywords failed!"), parent = e_cnd)
                   },
                   warning = function(w_cnd) {
                     cli::cli_inform(c("!" = "Checking DOES keywords resulted in warnings!"), parent = w_cnd)
                   })

  rlang::try_fetch(check_where_keys(steplist),
                   error = function(e_cnd) {
                     cli::cli_inform(c("x" = "Checking WHERE keywords failed!"), parent = e_cnd)
                   },
                   warning = function(w_cnd) {
                     cli::cli_inform(c("!" = "Checking WHERE keywords resulted in warnings!"), parent = w_cnd)
                   })

  rlang::try_fetch(check_module_keys(steplist),
                   error = function(e_cnd) {
                     cli::cli_inform(c("x" = "Checking Module keywords failed!"), parent = e_cnd)
                   },
                   warning = function(w_cnd) {
                     cli::cli_inform(c("!" = "Checking Module keywords resulted in warnings!"), parent = w_cnd)
                   })

  rlang::try_fetch(check_modules_in_steps(steplist),
                   error = function(e_cnd) {
                     cli::cli_inform(c("x" = "Checking Modules failed!"), parent = e_cnd)
                   },
                   warning = function(w_cnd) {
                     cli::cli_inform(c("!" = "Checking Modules resulted in warnings!"), parent = w_cnd)
                   })

  rlang::try_fetch(check_steps_in_icc(steplist),
                   error = function(e_cnd) {
                     cli::cli_inform(c("x" = "Checking ICC entries failed!"), parent = e_cnd)
                   })

  rlang::try_fetch(check_what_in_steps(steplist),
                   error = function(e_cnd) {
                     cli::cli_inform(c("x" = "Checking WHAT segments failed!"), parent = e_cnd)
                   },
                   warning = function(w_cnd) {
                     cli::cli_inform(c("!" = "Checking WHAT segments resulted in warnings!"), parent = w_cnd)
                   })

  rlang::try_fetch(check_does_in_steps(steplist),
                   error = function(e_cnd) {
                     cli::cli_inform(c("x" = "Checking DOES segments failed!"), parent = e_cnd)
                   },
                   warning = function(w_cnd) {
                     cli::cli_inform(c("!" = "Checking DOES segments resulted in warnings!"), parent = w_cnd)
                   })

  rlang::try_fetch(check_where_in_steps(steplist),
                   error = function(e_cnd) {
                     cli::cli_inform(c("x" = "Checking WHERE segments failed!"), parent = e_cnd)
                   },
                   warning = function(w_cnd) {
                     cli::cli_inform(c("!" = "Checking WHERE segments resulted in warnings!"), parent = w_cnd)
                   })

  rlang::try_fetch(check_refs(steplist),
                   error = function(e_cnd) {
                     cli::cli_inform(c("x" = "Checking references failed!"), parent = e_cnd)
                   },
                   warning = function(w_cnd) {
                     cli::cli_inform(c("!" = "Checking references resulted in warnings!"), parent = w_cnd)
                   })

  rlang::try_fetch(check_start_end_steps(steplist),
                   error = function(e_cnd) {
                     cli::cli_inform(c("x" = "Checking start/end steps failed!"), parent = e_cnd)
                   })

  rlang::try_fetch(check_then_use(steplist),
                   error = function(e_cnd) {
                     cli::cli_inform(c("x" = "Checking THEN statements failed!"), parent = e_cnd)
                   },
                   warning = function(w_cnd) {
                     cli::cli_inform(c("!" = "Checking THEN statements resulted in warnings!"), parent = w_cnd)
                   })

  rlang::try_fetch(check_then_if_ifnot(steplist),
                   error = function(e_cnd) {
                     cli::cli_inform(c("x" = "Checking THEN/IF/IFNOT equality failed!"), parent = e_cnd)
                   })

  rlang::try_fetch(check_outc(steplist),
                   error = function(e_cnd) {
                     cli::cli_inform(c("x" = "Checking outcome definitions failed!"), parent = e_cnd)
                   },
                   warning = function(w_cnd) {
                     cli::cli_inform(c("!" = "Checking outcome definitions resulted in warnings!"), parent = w_cnd)
                   })

  cli::cli_rule("Summary")

  # Class change if successful
  rlang::try_fetch({
    spsUtil::quiet(check_what_ids(steplist))
    spsUtil::quiet(check_does_ids(steplist))
    spsUtil::quiet(check_where_ids(steplist))
    spsUtil::quiet(check_module_ids(steplist))
    spsUtil::quiet(check_icc_ids(steplist))
    spsUtil::quiet(check_what_keys(steplist))
    spsUtil::quiet(check_does_keys(steplist))
    spsUtil::quiet(check_where_keys(steplist))
    spsUtil::quiet(check_module_keys(steplist))
    spsUtil::quiet(check_modules_in_steps(steplist))
    spsUtil::quiet(check_steps_in_icc(steplist))
    spsUtil::quiet(check_what_in_steps(steplist))
    spsUtil::quiet(check_does_in_steps(steplist))
    spsUtil::quiet(check_where_in_steps(steplist))
    spsUtil::quiet(check_refs(steplist))
    spsUtil::quiet(check_start_end_steps(steplist))
    spsUtil::quiet(check_then_use(steplist))
    spsUtil::quiet(check_then_if_ifnot(steplist))
    spsUtil::quiet(check_outc(steplist))
    steplist %<>% unclass(.)
    steplist %<>% structure(., class = "epicmodel_steplist_checked")
    cli::cli_alert_success("Checking successful!")
    },
    error = function(cnd) {
      cli::cli_alert_danger("Checking failed! Please correct errors and repeat.")
    }
  )

  invisible(steplist)
}
