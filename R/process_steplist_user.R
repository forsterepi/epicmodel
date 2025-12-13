#' Unchecking `epicmodel_steplist` objects
#'
#' Putting a checked `epicmodel_steplist` back to an unchecked status.
#'
#' @param steplist An `epicmodel_steplist` or `epicmodel_steplist_checked` object.
#'
#' @returns An object of class `epicmodel_steplist`.
#'
#' @export
#'
#' @examples
#' x <- uncheck_steplist(scc_rain$steplist)
uncheck_steplist <- function(steplist) {
  # Check input
  if (inherits(steplist, c("epicmodel_steplist_checked", "epicmodel_steplist")) %>% magrittr::not()) {
    cli::cli_abort("Input validation error: {.var steplist}", .internal = TRUE, class = "input_steplist")
  }
  #=============================================================================
  if (inherits(steplist, "epicmodel_steplist_checked")) {
    steplist %<>% unclass(.)
    steplist %<>% structure(., class = "epicmodel_steplist")
  }
  #=============================================================================
  # Check output
  if (inherits(steplist, "epicmodel_steplist") %>% magrittr::not()) {
    cli::cli_abort("Output validation error: {.var steplist}", .internal = TRUE, class = "output_steplist")
  }
  #=============================================================================
  return(steplist)
}

#' Remove all modules
#'
#' Removes all entries in data.frame `module` from an `epicmodel_steplist` object. Also turns all values of variable `module_step` in data.frame
#' `step` from an `epicmodel_steplist` to empty strings.
#'
#' @param steplist An `epicmodel_steplist` or `epicmodel_steplist_checked` object.
#'
#' @returns An `epicmodel_steplist` object with empty data.frame `module` and empty strings in variable `module_step` in data.frame `step`. When
#'   continuing with this steplist, SCC models cannot be inspected by module. If you made any changes, you need to call [check_steplist()] again.
#' @export
#'
#' @examples
#' x <- remove_all_modules(steplist_party)
remove_all_modules <- function(steplist) {
  # Check input
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })
  #=============================================================================
  # Remove specified modules from steplist$step
  steplist$step$module_step <- ""

  # Remove modules from steplist$module
  steplist$module %<>% dplyr::filter(.data$id_module == "")

  # Warn that check_steplist() needs to be repeated
  if (inherits(steplist, "epicmodel_steplist_checked")) {
    cli::cli_alert_warning("Changing the steplist makes it necessary to repeat {.code check_steplist()}!")
  }

  # Uncheck
  steplist %<>% uncheck_steplist()
  #=============================================================================
  # Check output
  rlang::try_fetch({
      checkmate::assert_class(steplist, "epicmodel_steplist")
      validate_steplist(steplist)
    }, error = function(cnd) {cli::cli_abort("Output validation error: {.var steplist}", .internal = TRUE,
                                               parent = cnd, class = "output")
  })
  #=============================================================================
  return(steplist)
}

#' Removing NA in `icc` and `outc`
#'
#' Remove any entries that only consist of NA from data.frames `icc` (Incompatible Component Causes) and `outc` (outcome definition) from an
#' `epicmodel_steplist`.
#'
#' @param steplist An `epicmodel_steplist` or `epicmodel_steplist_checked` object.
#'
#' @returns An `epicmodel_steplist` object without entries in data.frame `icc`, which contain 'NA' in either `id1` or `id2` as well as entries in
#'   data.frame `outc` that contain 'NA' in `id_outc`. If you made any changes, you need to call [check_steplist()] again.
#' @export
#'
#' @examples
#' x <- remove_na(steplist_party)
remove_na <- function(steplist) {
  # Check input
  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })
  #=============================================================================
  # Remove NA from icc
  steplist$icc %<>% dplyr::filter(.data$id1 != "NA" & .data$id2 != "NA")

  # Remove NA from outc
  steplist$outc %<>% dplyr::filter(.data$id_outc != "NA")

  # Warn that check_steplist() needs to be repeated
  if (inherits(steplist, "epicmodel_steplist_checked")) {
    cli::cli_alert_warning("Changing the steplist makes it necessary to repeat {.code check_steplist()}!")
  }

  # Uncheck
  steplist %<>% uncheck_steplist()
  #=============================================================================
  # Check output
  rlang::try_fetch({
      checkmate::assert_class(steplist, "epicmodel_steplist")
      validate_steplist(steplist)
    }, error = function(cnd) {cli::cli_abort("Output validation error: {.var steplist}", .internal = TRUE,
                                             parent = cnd, class = "output")
  })
  #=============================================================================
  return(steplist)
}

#' Remove segments
#'
#' Removes individual entries from data.frames `what`, `does`, `where`, `module`, or `icc`.
#'
#' @param steplist An `epicmodel_steplist` or `epicmodel_steplist_checked` object.
#' @param id A single non-missing element of type character describing the ID of the entry you want deleted.
#'
#' @returns An `epicmodel_steplist` class object. If you made any changes, you need to call [check_steplist()] again.
#' @export
#'
#' @examples
#' steplist_party <- remove_segment(steplist_party, "d4")
remove_segment <- function(steplist, id) {
  # Check inputs
  rlang::try_fetch(checkmate::assert_character(id, min.len = 1, any.missing = F, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var id} must be a single element of type character!",
                                    parent = cnd, class = "no_character")
                   })

  rlang::try_fetch(checkmate::assert_character(id, len = 1, any.missing = F, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("Please only specify a single {.var id}!",
                                    parent = cnd, class = "no_single_character")
                   })

  rlang::try_fetch(validate_steplist(steplist),
                   error = function(cnd) {
                     cli::cli_abort("{.var steplist} must be an object of class {.emph epicmodel_steplist}!",
                                    parent = cnd, class = "no_epicmodel_steplist")
                   })
  #=============================================================================
  # Define pattern of valid segments
  pattern <- "^[adeim][:digit:]+$"

  # Get table based on pattern
  if (id %>% stringr::str_detect(pattern)) {
    letter <- id %>% stringr::str_sub(1,1)
    steplist_part <- switch(letter,
                            "a" = "what",
                            "d" = "does",
                            "e" = "where",
                            "i" = "icc",
                            "m" = "module")
    steplist_id <- paste0("id_",steplist_part)
    ## Check availability
    if (id %>% magrittr::is_in(steplist[[steplist_part]][[steplist_id]]) %>% magrittr::not()) {
      cli::cli_abort("{.var id} is not available!", class = "id_not_found")
    }
    # Remove
    steplist[[steplist_part]] %<>% dplyr::filter(.data[[steplist_id]] != id)
  } else {
    cli::cli_abort(c("{.var id} was not specified correctly!",
                     "i" = "This function can only be used to delete from data.frames {.var what}, {.var does}, {.var where},
                     {.var module} & {.var icc}."),
                   class = "wrong_pattern")
  }

  # Warn that check_steplist() needs to be repeated
  if (inherits(steplist, "epicmodel_steplist_checked")) {
    cli::cli_alert_warning("Changing the steplist makes it necessary to repeat {.code check_steplist()}!")
  }

  # Uncheck
  steplist %<>% uncheck_steplist()
  #=============================================================================
  # Check output
  rlang::try_fetch({
      checkmate::assert_class(steplist, "epicmodel_steplist")
      validate_steplist(steplist)
    }, error = function(cnd) {cli::cli_abort("Output validation error: {.var steplist}", .internal = TRUE,
                                             parent = cnd, class = "output")
  })
  #=============================================================================
  return(steplist)
}
