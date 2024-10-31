#' Separate steps into its parts
#'
#' @param x A vector of type character containing one step ID per element.
#'
#' @return A named list of length 3 with names 'if', 'ifnot', and 'then', each containing a character vector. If a certain part is not available, the
#'   value is NA.
#'
#' @noRd
sep_step <- function(x) {
  # Check input
  rlang::try_fetch({
      checkmate::assert_character(x)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var x}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input")
  })
  #=============================================================================
  # Create empty container
  out <- vector(mode = "list", length = 3) %>% magrittr::set_names(c("if","ifnot","then"))

  # Check which elements are present
  contains_ifnot <- x %>% stringr::str_detect("IFNOT")
  if (x %>% is.na() %>% magrittr::not() %>% sum() %>% magrittr::is_greater_than(0)) {
    if_ends_with <- ifelse(contains_ifnot, "IFNOT", "THEN")
  } else {
    if_ends_with <- "THEN"
  }

  # Separate
  start_if <- x %>% stringr::str_locate("IF(?!N)") %>% magrittr::extract(,"end") %>% magrittr::add(1)
  end_if <- x %>% stringr::str_locate(if_ends_with) %>% magrittr::extract(,"start") %>% magrittr::subtract(1)
  out[["if"]] <- x %>% stringr::str_sub(start_if, end_if) %>% stringr::str_trim("both")

  start_ifnot <- x %>% stringr::str_locate("IFNOT") %>% magrittr::extract(,"end") %>% magrittr::add(1)
  end_ifnot <- x %>% stringr::str_locate("THEN") %>% magrittr::extract(,"start") %>% magrittr::subtract(1)
  out[["ifnot"]] <- x %>% stringr::str_sub(start_ifnot, end_ifnot) %>% stringr::str_trim("both")

  start_then <- x %>% stringr::str_locate("THEN") %>% magrittr::extract(,"end") %>% magrittr::add(1)
  out[["then"]] <- x %>% stringr::str_sub(start_then, -1) %>% stringr::str_trim("both")

  # Return
  return(out)
}

#' Separate IF/IFNOT into its parts
#'
#' @param x A vector of type character containing an IF conditions or an IFNOT condition.
#'
#' @return A data.frame containing one THEN statement per row with two columns:
#' * sce: The scenario of the THEN statement, which is an integer but of type character. Statments in the same scenario are combined with AND,
#'   while all statements in different scenarios are combined with OR.
#' * id: The ID of the corresponding THEN statement. If `x` is NA, the function returns a list of length 1 with a table containing 1 row of NAs.
#'   If `x` is an empty character vector, the function returns a list of length 1 with a table containing 0 rows.
#'
#' @noRd
sep_if_ifnot <- function(x) {
  # Check input
  rlang::try_fetch({
      checkmate::assert_character(x)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var x}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input")
  })
  #=============================================================================
  # Check if there is input in x
  if (length(x) > 0) {
    ## Split scenarios
    split <- x %>% stringr::str_split("or")
    ## Create empty container
    out <- vector(mode = "list", length = length(x))
    ## Loop over every entry of x (vectorized function)
    for (i in 1:length(split)) {
      ### split elements in scenario
      temp <- split[[i]] %>%
        stringr::str_replace("^\\(","") %>%
        stringr::str_replace("\\)$","") %>%
        stringr::str_split("\\+")
      ### Check for NAs
      if (temp %>% is.na() %>% sum() %>% magrittr::equals(0)) {
        #### Create container for elements of scenarios
        out_by_row <- vector(mode = "list", length = length(temp))
        #### Loop over scenarios
        for (j in 1:length(out_by_row)) {
          ##### Get number of elements inside scenario
          n_row <- temp[[j]] %>% length()
          ##### Create empty part for output table
          out_temp <- matrix(rep(NA, n_row*2), nrow = n_row, ncol = 2) %>%
            as.data.frame() %>%
            magrittr::set_colnames(c("sce","id"))
          ##### Fill output table
          out_temp$sce <- as.character(j)
          out_temp$id <- temp[[j]]
          ##### Put into container
          out_by_row[[j]] <- out_temp
        }
        #### Combine all parts to one data.frame
        out_by_row %<>% purrr::map_dfr(as.data.frame)
      } else {
        out_by_row <- data.frame(sce = NA, id = NA)
      }
      ### Combine all tables from all elements of x
      out[[i]] <- out_by_row
    }
  } else{
    out <- matrix(rep("NA", 2), nrow = 1, ncol = 2) %>%
      as.data.frame() %>%
      magrittr::set_colnames(c("sce","id"))
    out %<>% dplyr::filter(.data$sce != "NA")
    out <- list(out)
  }

  # Return
  return(out)
}

#' Separate THEN statements into its parts
#'
#' @param x A vector of type character containing one THEN statement ID per element, but without the keyword 'THEN'.
#'
#' @return A named list of length 4 with names 'subject', 'does', 'object', and 'where', each containing a character vector. If a certain segment
#' is not available, the value is NA.
#'
#' @noRd
sep_then <- function(x) {
  # Check input
  rlang::try_fetch({
      checkmate::assert_character(x)
    }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var x}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "input")
  })
  #=============================================================================
  # Create empty container
  out <- vector(mode = "list", length = 4) %>% magrittr::set_names(c("subject","does","object","where"))

  # Check if step IDs have been provided instead of THEN statements
  if (x %>% is.na() %>% magrittr::not() %>% sum() %>% magrittr::is_greater_than(0)) {
    contains_if <- x %>% stringr::str_detect("IF(?!N)") %>% sum(na.rm = T) %>% magrittr::is_greater_than(0)
    contains_ifnot <- x %>% stringr::str_detect("IFNOT") %>% sum(na.rm = T) %>% magrittr::is_greater_than(0)
    contains_then <- x %>% stringr::str_detect("THEN") %>% sum(na.rm = T) %>% magrittr::is_greater_than(0)
    if (contains_if | contains_ifnot | contains_then) {
      error_hint <- c("IF","IFNOT","THEN")[c(contains_if,contains_ifnot,contains_then)] %>% stringr::str_c(collapse = ", ")
        cli::cli_abort(c("{.var x} can't contain {.emph IF}, {.emph IFNOT}, or {.emph THEN}.",
                       "x" = "You used {.emph {error_hint}}."),
                     class = "sep_then_keyword")
    }
  }

  # Is there a THEN object?
  then_object <- x %>% stringr::str_detect("\\(") & x %>% stringr::str_detect("\\)")

  # WHERE
  start_where_with_then <- x %>% stringr::str_locate("\\)") %>% magrittr::extract(,"end") %>% magrittr::add(1)
  where_area <- ifelse(then_object,
                       x %>% stringr::str_sub(start_where_with_then,-1),
                       x)
  out[["where"]] <- where_area %>% stringr::str_extract("e[:digit:]+(?![:digit:])")

  # DOES
  end_does_with_then <- x %>% stringr::str_locate("\\(") %>% magrittr::extract(,"start") %>% magrittr::subtract(1)
  does_area <- ifelse(then_object,
                       x %>% stringr::str_sub(1,end_does_with_then),
                       x)
  out[["does"]] <- does_area %>% stringr::str_extract("d[:digit:]+(?![:digit:])")
  contains_does <- out[["does"]] %>% is.na() %>% magrittr::not()

  # Subject
  end_subject_with_then <- x %>% stringr::str_locate("\\(") %>% magrittr::extract(,"start") %>% magrittr::subtract(1)
  end_subject_without_then <- x %>% stringr::str_locate("d") %>% magrittr::extract(,"start") %>% magrittr::subtract(1)
  subject_area <- ifelse(then_object,
                         x %>% stringr::str_sub(1,end_subject_with_then),
                         ifelse(contains_does,
                                x %>% stringr::str_sub(1,end_subject_without_then),
                                x))
  out[["subject"]] <- subject_area %>% stringr::str_extract("a[:digit:]+(?![:digit:])")

  # Object
  start_object_without_then <- x %>% stringr::str_locate("d[:digit:]+(?![:digit:])") %>% magrittr::extract(,"end") %>% magrittr::add(1)
  start_then <- x %>% stringr::str_locate("\\(") %>% magrittr::extract(,"end") %>% magrittr::add(1)
  end_then <- x %>% stringr::str_locate("\\)") %>% magrittr::extract(,"start") %>% magrittr::subtract(1)

  contains_2_what <- x %>% stringr::str_extract_all("a[:digit:]+(?![:digit:])", simplify = T)
  if (contains_2_what %>% ncol() %>% magrittr::is_less_than(2)) {
    contains_2_what %<>% cbind(., rep("",nrow(.)), rep("",nrow(.)))
  }
  contains_2_what %<>% magrittr::extract(,2) %>% magrittr::equals("") %>% magrittr::not()
  end_first_a <- x %>% stringr::str_locate("a[:digit:]+(?![:digit:])") %>% magrittr::extract(,"end") %>% magrittr::add(1)

  object_area <- ifelse(then_object,
                        x,
                        ifelse(contains_does,
                               x %>% stringr::str_sub(start_object_without_then,-1),
                               ifelse(contains_2_what,
                                      x %>% stringr::str_sub(end_first_a,-1),
                                      "")))
  out[["object"]] <- ifelse(then_object,
                            x %>% stringr::str_sub(start_then,end_then),
                            object_area %>% stringr::str_extract("a[:digit:]+(?![:digit:])"))

  if (out[["object"]] %>% typeof() %>% magrittr::equals("logical")) {
    out[["object"]] %<>% as.character(.)
  }

  # Return
  return(out)
}

#' Process steplist
#'
#' @param x A `epicmodel_steplist` clas object, of which the element `step` will be processed.
#'
#' @return A tibble containing processed steps
#'
#' @noRd
process_steplist <- function(steplist) {
  # Check input
  ## Must be possible to process un-checked steplists because processing is part of checking
  rlang::try_fetch({
    steplist %<>% validate_steplist()
  }, error = function(cnd) {cli::cli_abort(c("Input validation error: {.var steplist}",
                                             "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                           parent = cnd, class = "input")
  })
  #=============================================================================
  # Select relevant steps from steplist$step
  steps <- steplist$step %>% dplyr::select(dplyr::all_of(c("id_step","end_step","module_step"))) %>% tidyr::as_tibble()

  # Separate step IDs
  steps %<>% dplyr::mutate(if_step = sep_step(.data$id_step)[["if"]])
  steps %<>% dplyr::mutate(ifnot_step = sep_step(.data$id_step)[["ifnot"]])
  steps %<>% dplyr::mutate(then_step = sep_step(.data$id_step)[["then"]])

  # Separate THEN statements
  steps %<>% dplyr::mutate(subject_step = sep_then(.data$then_step)[["subject"]])
  steps %<>% dplyr::mutate(does_step = sep_then(.data$then_step)[["does"]])
  steps %<>% dplyr::mutate(object_step = sep_then(.data$then_step)[["object"]])
  steps %<>% dplyr::mutate(where_step = sep_then(.data$then_step)[["where"]])

  # Separate IF/IFNOT conditions
  steps %<>% dplyr::mutate(if_list = sep_if_ifnot(.data$if_step))
  steps %<>% dplyr::mutate(ifnot_list = sep_if_ifnot(.data$ifnot_step))

  # Re-order
  steps %<>% dplyr::select(dplyr::all_of(c("id_step",
                                    "then_step","subject_step","does_step","object_step","where_step",
                                    "if_step","if_list",
                                    "ifnot_step","ifnot_list",
                                    "end_step", "module_step")))

  # Trim
  steps$end_step %<>% stringr::str_trim("both")
  steps$module_step %<>% stringr::str_trim("both")

  # Return
  return(steps)
}
