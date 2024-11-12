#' Create WHAT, DOES, WHERE, Module & ICC IDs
#'
#' Automatically creates the next ID for datasets WHAT, DOES, WHERE, MODULE & ICC in the Steplist Creator `shiny` app.
#'
#' @param id_col A character vector. The existing list of IDs from the WHAT, DOES, WHERE, Module, or ICC table, from which the next ID is concluded.
#' @param steplist_part A single element from the following options:'what','does','where','module','icc'. If the corresponding data.frame behind
#' id_col is empty, the first ID is created for the specified steplist_part, e.g., 'a1' for steplist_part 'what'.
#'
#' @returns A single element of type character containing the next ID in the input column.
#'
#' @noRd
fun_get_id <- function(id_col, steplist_part = c("what","does","where","module","icc")){

  steplist_part <- match.arg(steplist_part)

  # Handle "", NA, NULL, other non-character inputs
  if (is.null(id_col)) {
    id_col <- character(0)
  }
  if (!is.character(id_col)) {
      id_col <- character(0)
  }
  if (length(id_col) == 1) {
    if (id_col == "") {
      id_col <- character(0)
    }}

  # Get letter, i.e., first character of ID (WHAT -> a, DOES -> d, WHERE -> e, Module -> m, ICC -> i)
  if (length(id_col) == 0) {
    letter_to_use <- switch(steplist_part,
      "what" = "a",
      "does" = "d",
      "where" = "e",
      "module" = "m",
      "icc" = "i"
    )
  } else {
    letter_to_use <- id_col %>% stringr::str_sub(1,1) %>% unique()
  }

  # Get newest number
  if (length(id_col) == 0) {
    number_to_use <- "1"
  } else {
    number_to_use <- id_col %>% stringr::str_sub(2,-1) %>% as.numeric() %>% max(na.rm = T) %>% magrittr::add(1)
  }
  ## Create ID
  out <- paste0(letter_to_use,number_to_use)

  # Return
  return(out)
}

#' Create THEN IDs
#'
#' Automatically creates the ID for THEN statements in the Steplist Creator `shiny` app.
#'
#' @param subject_key Single element of type character. The keyword from the WHAT (Subject) dropdown menu.
#' @param does_key Single element of type character. The keyword from the DOES dropdown menu.
#' @param object_key Single element of type character. The keyword from the WHAT (Object) or THEN (Object) dropdown menu.
#' @param where_key Single element of type character. The keyword from the WHERE dropdown menu.
#' @param check_object Single element of type character, either "" or "1". Indicator if object is a WHAT segment ("") or a THEN statement ("1").
#'   Default is "".
#' @param what_data A data.frame. The current state of the list of WHAT segments.
#' @param does_data A data.frame. The current state of the list of DOES segments.
#' @param where_data A data.frame. The current state of the list of WHERE segments.
#' @param then_data A data.frame. The current state of the list of THEN statements.
#'
#' @returns A single element of type character containing the automatically created ID of the THEN statement.
#'
#' @noRd
fun_create_then_step_id <- function(subject_key, does_key, object_key, where_key, check_object = "", what_data, does_data, where_data, then_data){

  # Placeholders to NA
  if (is.null(subject_key)) {
    subject_key <- ""
  }
  if (subject_key %>% magrittr::is_in(c("NA","na",""," "))) {
    subject_key <- NA
  }

  if (is.null(does_key)) {
    does_key <- ""
  }
  if (does_key %>% magrittr::is_in(c("NA","na",""," "))) {
    does_key <- NA
  }

  if (is.null(object_key)) {
    object_key <- ""
  }
  if (object_key %>% magrittr::is_in(c("NA","na",""," "))) {
    object_key <- NA
  }

  if (is.null(where_key)) {
    where_key <- ""
  }
  if (where_key %>% magrittr::is_in(c("NA","na",""," "))) {
    where_key <- NA
  }

  # Get IDs
  if (is.na(subject_key)) {
    subject_id <- ""
  } else {
    temp <- what_data %>% dplyr::filter(.data$key_what == subject_key)
    if (temp %>% nrow() %>% magrittr::is_greater_than(1)) {
      cli::cli_abort("The subject keyword appears multiple times. Please delete duplicate entries.")
    }
    subject_id <- what_data$id_what[what_data$key_what == subject_key & !is.na(what_data$key_what)]
  }

  if (is.na(does_key)) {
    does_id <- ""
  } else {
    temp <- does_data %>% dplyr::filter(.data$key_does == does_key)
    if (temp %>% nrow() %>% magrittr::is_greater_than(1)) {
      cli::cli_abort("The DOES keyword appears multiple times. Please delete duplicate entries.")
    }
    does_id <- does_data$id_does[does_data$key_does == does_key & !is.na(does_data$key_does)]
  }

  if (is.na(object_key)) {
    object_id <- ""
  } else {
    if (check_object == "1") {
      temp <- then_data %>% dplyr::filter(.data$desc_then == object_key)
      if (temp %>% nrow() %>% magrittr::is_greater_than(1)) {
        cli::cli_abort("The object keyword appears multiple times. Please delete duplicate entries.")
      }
      object_id <- paste0("(",then_data$id_then[then_data$desc_then == object_key & !is.na(then_data$desc_then)],")")
    } else {
      temp <- what_data %>% dplyr::filter(.data$key_what == object_key)
      if (temp %>% nrow() %>% magrittr::is_greater_than(1)) {
        cli::cli_abort("The object keyword appears multiple times. Please delete duplicate entries.")
      }
      object_id <- what_data$id_what[what_data$key_what == object_key & !is.na(what_data$key_what)]
    }
  }

  if (is.na(where_key)) {
    where_id <- ""
  } else {
    temp <- where_data %>% dplyr::filter(.data$key_where == where_key)
    if (temp %>% nrow() %>% magrittr::is_greater_than(1)) {
      cli::cli_abort("The where keyword appears multiple times. Please delete duplicate entries.")
    }
    where_id <- where_data$id_where[where_data$key_where == where_key & !is.na(where_data$key_where)]
  }

  # Combine IDs
  then_id <- paste0(subject_id,does_id,object_id,where_id)

  # Return
  return(then_id)
}

#' Create THEN descriptions
#'
#' Automatically creates the description for THEN statements in the Steplist Creator `shiny` app.
#'
#' @param subject_key Single element of type character. The keyword from the WHAT (Subject) dropdown menu.
#' @param does_key Single element of type character. The keyword from the DOES dropdown menu.
#' @param object_key Single element of type character. The keyword from the WHAT (Object) or THEN (Object) dropdown menu.
#' @param where_key Single element of type character. The keyword from the WHERE dropdown menu.
#' @param check_object Single element of type character, either "" or "1". Indicator if object is a WHAT segment ("") or a THEN statement ("1").
#'   Default is "".
#' @param what_data A data.frame. The current state of the list of WHAT segments.
#' @param does_data A data.frame. The current state of the list of DOES segments.
#' @param where_data A data.frame. The current state of the list of WHERE segments.
#' @param then_data A data.frame. The current state of the list of THEN statements.
#'
#' @returns A single element of type character containing the automatically created description of the THEN statement.
#'
#' @noRd
fun_create_then_step_desc <- function(subject_key, does_key, object_key, where_key, check_object = "", what_data, does_data, where_data, then_data){

  # Placeholders to NA
  if (is.null(subject_key)) {
    subject_key <- ""
  }
  if (subject_key %>% magrittr::is_in(c("NA","na",""," "))) {
    subject_key <- NA
  }

  if (is.null(does_key)) {
    does_key <- ""
  }
  if (does_key %>% magrittr::is_in(c("NA","na",""," "))) {
    does_key <- NA
  }

  if (is.null(object_key)) {
    object_key <- ""
  }
  if (object_key %>% magrittr::is_in(c("NA","na",""," "))) {
    object_key <- NA
  }

  if (is.null(where_key)) {
    where_key <- ""
  }
  if (where_key %>% magrittr::is_in(c("NA","na",""," "))) {
    where_key <- NA
  }

  # Get IDs
  if (is.na(subject_key)) {
    subject_desc <- ""
  } else {
    subject_desc <- what_data$desc_what[what_data$key_what == subject_key & !is.na(what_data$key_what)]
  }

  if (is.na(does_key)) {
    does_desc <- ""
  } else {

    if (is.na(subject_key)) {
      does_desc <- does_data$no_subject_does[does_data$key_does == does_key & !is.na(does_data$key_does)]
    } else {
      if (!is.na(what_data$plural_what[what_data$key_what == subject_key & !is.na(what_data$key_what)])) {
        if (what_data$plural_what[what_data$key_what == subject_key & !is.na(what_data$key_what)] == "1") {
          does_desc <- does_data$subject_plural_does[does_data$key_does == does_key & !is.na(does_data$key_does)]
        }
        if (what_data$plural_what[what_data$key_what == subject_key & !is.na(what_data$key_what)] == "0") {
          does_desc <- does_data$subject_singular_does[does_data$key_does == does_key & !is.na(does_data$key_does)]
        }
      } else {
        cli::cli_abort("The corresponding information on plural/singular is missing for the subject. Please add to the data.")
      }
    }
  }

  if (is.na(object_key)) {
    object_desc <- ""
  } else {
    if (check_object == "1") {
      object_desc <- object_key
    } else {
      object_desc <- what_data$desc_what[what_data$key_what == object_key & !is.na(what_data$key_what)]
    }
  }

  if (is.na(where_key)) {
    where_desc <- ""
  } else {
    where_desc <- where_data$desc_where[where_data$key_where == where_key & !is.na(where_data$key_where)]
  }

  # Combine IDs
  if (check_object == "1") {
    then_desc <- c(subject_desc,does_desc,"that",object_desc,where_desc) %>% .[. != ""] %>% stringr::str_c(collapse = " ")
  } else {
    then_desc <- c(subject_desc,does_desc,object_desc,where_desc) %>% .[. != ""] %>% stringr::str_c(collapse = " ")
  }

  # Return
  return(then_desc)
}

#' Create IFNOT IDs
#'
#' Automatically creates the ID for IFNOT conditions in the Steplist Creator `shiny` app.
#'
#' @param input_select A list of all inputs from the IFNOT dropdown menus.
#' @param input_numeric A list of all inputs from the IFNOT numerical inputs, which indicate AND/OR logic.
#' @param then_data A data.frame. The current state of the list of THEN statements.
#'
#' @returns A list of length 3 containing i) the ID of the IFNOT condition as character, ii) a boolean indicator, which is TRUE if some scenario
#' (same number, i.e., combined with AND) of the IFNOT condition contains duplicate statements, iii) a boolean indicator, which is TRUE if some
#' scenarios of the IFNOT condition are identical.
#'
#' @noRd
fun_create_step_ifnot_id <- function(input_select, input_numeric, then_data){
  # Create default states
  check_dupli <- F
  check_dupli2 <- F

  # Combine and cut down
  input_select %<>% unlist()
  input_numeric %<>% unlist()

  input <- cbind(input_numeric,input_select) %>% as.data.frame()
  input %<>% dplyr::filter(.data$input_select != "")
  input$input_numeric %<>% as.numeric()

  # Check duplicates
  if (nrow(input) > 0) {

    input$dupli <- input[,c("input_select","input_numeric")] %>% duplicated()

    if (input$dupli %>% sum() %>% magrittr::is_greater_than(0)) {
      check_dupli <- T
    }

    # Add IDs
    input %<>% dplyr::left_join(then_data, by = c("input_select" = "desc_then"))

    # Adjust input_numeric
    adj <- input$input_numeric %>% unique() %>% cbind(., c(1:length(.))) %>%
      as.data.frame() %>% magrittr::set_colnames(c("input_numeric","adjusted_input_numeric"))

    input %<>% dplyr::left_join(adj, by = "input_numeric")

    # Paste together
    max_scen <- input$adjusted_input_numeric %>% max()
    out <- vector(mode = "character", length = max_scen)

    for (i in 1:max_scen) {
      temp <- input %>% dplyr::filter(.data$adjusted_input_numeric == i)
      out[i] <- temp$id_then %>% stringr::str_c(collapse = "+")
    }

    if (length(out) > 1) {
      if (out %>% duplicated() %>% sum() %>% magrittr::is_greater_than(0)) {
        check_dupli2 <- T
      }
      out %<>% stringr::str_c(collapse = ")or(")
      out %<>% paste0("(",.,")")
    }

    # Return
    return(list(out,check_dupli,check_dupli2))
  } else {
    out <- ""
    return(list(out,check_dupli,check_dupli2))
  }
}

#' Create IFNOT descriptions
#'
#' Automatically creates the description for IFNOT conditions in the Steplist Creator `shiny` app.
#'
#' @param input_select A list of all inputs from the IFNOT dropdown menus.
#' @param input_numeric A list of all inputs from the IFNOT numerical inputs, which indicate AND/OR logic.
#'
#' @returns A single element of type character containing the automatically created description of the IFNOT condition.
#'
#' @noRd
fun_create_step_ifnot_desc <- function(input_select, input_numeric){
  # Combine and cut down
  input_select %<>% unlist()
  input_numeric %<>% unlist()

  input <- cbind(input_numeric,input_select) %>% as.data.frame()
  input %<>% dplyr::filter(.data$input_select != "")
  input$input_numeric %<>% as.numeric()

  # Adjust input_numeric
  if (nrow(input) > 0) {

    adj <- input$input_numeric %>% unique() %>% cbind(., c(1:length(.))) %>%
      as.data.frame() %>% magrittr::set_colnames(c("input_numeric","adjusted_input_numeric"))

    input %<>% dplyr::left_join(adj, by = "input_numeric")

    # Paste together
    max_scen <- input$adjusted_input_numeric %>% max()
    out <- vector(mode = "character", length = max_scen)

    for (i in 1:max_scen) {
      temp <- input %>% dplyr::filter(.data$adjusted_input_numeric == i)
      out[i] <- temp$input_select %>% stringr::str_c(collapse = " and ")

      if (temp %>% nrow() %>% magrittr::is_greater_than(1) & max_scen %>% magrittr::is_greater_than(1)) {
        out[i] %<>% paste0("(",.,")")
      }
    }

    out %<>% stringr::str_c(collapse = " or ")

    # Return
    return(out)
  } else {
    out <- ""
    return(out)
  }
}

#' Create IF IDs
#'
#' Automatically creates the ID for IF conditions in the Steplist Creator `shiny` app.
#'
#' @param input_select A list of all inputs from the IF dropdown menus.
#' @param input_numeric A list of all inputs from the IF numerical inputs, which indicate AND/OR logic.
#' @param then_data A data.frame. The current state of the list of THEN statements.
#'
#' @returns A list of length 3 containing i) the ID of the IF condition as character, ii) a boolean indicator, which is TRUE if some scenario (same
#'   number, i.e., combined with AND) of the IF condition contains duplicate statements, iii) a boolean indicator, which is TRUE if some scenarios of
#'   the IF condition are identical.
#'
#' @noRd
fun_create_step_if_id <- function(input_select, input_numeric, then_data){
  # Create default states
  check_dupli <- F
  check_dupli2 <- F

  # Combine and cut down
  input_select %<>% unlist()
  input_numeric %<>% unlist()

  input <- cbind(input_numeric,input_select) %>% as.data.frame()
  input %<>% dplyr::filter(.data$input_select != "")
  input$input_numeric %<>% as.numeric()

  # Check duplicates
  if (nrow(input) > 0) {

    input$dupli <- input[,c("input_select","input_numeric")] %>% duplicated()

    if (input$dupli %>% sum() %>% magrittr::is_greater_than(0)) {
      check_dupli <- T
    }

    # Add IDs
    input %<>% dplyr::left_join(then_data, by = c("input_select" = "desc_then"))

    # Adjust input_numeric
    adj <- input$input_numeric %>% unique() %>% cbind(., c(1:length(.))) %>%
      as.data.frame() %>% magrittr::set_colnames(c("input_numeric","adjusted_input_numeric"))

    input %<>% dplyr::left_join(adj, by = "input_numeric")

    # Paste together
    max_scen <- input$adjusted_input_numeric %>% max()
    out <- vector(mode = "character", length = max_scen)

    for (i in 1:max_scen) {
      temp <- input %>% dplyr::filter(.data$adjusted_input_numeric == i)
      out[i] <- temp$id_then %>% stringr::str_c(collapse = "+")
    }

    if (length(out) > 1) {
      if (out %>% duplicated() %>% sum() %>% magrittr::is_greater_than(0)) {
        check_dupli2 <- T
      }
      out %<>% stringr::str_c(collapse = ")or(")
      out %<>% paste0("(",.,")")
    }

    # Return
    return(list(out,check_dupli,check_dupli2))
  } else {
    out <- ""
    return(list(out,check_dupli,check_dupli2))
  }
}

#'Create IF descriptions
#'
#'Automatically creates the description for IF conditions in the Steplist Creator `shiny` app.
#'
#'@param input_select A list of all inputs from the IF dropdown menus.
#'@param input_numeric A list of all inputs from the IF numerical inputs, which indicate AND/OR logic.
#'
#'@returns A single element of type character containing the automatically created description of the IF condition.
#'
#'@noRd
fun_create_step_if_desc <- function(input_select, input_numeric){
  # Combine and cut down
  input_select %<>% unlist()
  input_numeric %<>% unlist()

  input <- cbind(input_numeric,input_select) %>% as.data.frame()
  input %<>% dplyr::filter(.data$input_select != "")
  input$input_numeric %<>% as.numeric()

  # Adjust input_numeric
  if (nrow(input) > 0) {

    adj <- input$input_numeric %>% unique() %>% cbind(., c(1:length(.))) %>%
      as.data.frame() %>% magrittr::set_colnames(c("input_numeric","adjusted_input_numeric"))

    input %<>% dplyr::left_join(adj, by = "input_numeric")

    # Paste together
    max_scen <- input$adjusted_input_numeric %>% max()
    out <- vector(mode = "character", length = max_scen)

    for (i in 1:max_scen) {
      temp <- input %>% dplyr::filter(.data$adjusted_input_numeric == i)
      out[i] <- temp$input_select %>% stringr::str_c(collapse = " and ")

      if (temp %>% nrow() %>% magrittr::is_greater_than(1) & max_scen %>% magrittr::is_greater_than(1)) {
        out[i] %<>% paste0("(",.,")")
      }
    }

    out %<>% stringr::str_c(collapse = " or ")

    # Return
    return(out)
  } else {
    out <- ""
    return(out)
  }
}

#' Create step IDs
#'
#' Automatically creates the ID for steps in the Steplist Creator `shiny` app.
#'
#' @param input_if A single element of type character. The ID of the IF condition as provided in the first element of the output of
#' `fun_create_step_if_id()`.
#' @param input_ifnot A single element of type character. The ID of the IFNOT condition as provided in the first element of the output of
#'   `fun_create_step_ifnot_id()`.
#' @param input_then A single element of type character. The ID of the THEN statement as provided in the output of `fun_create_then_step_id()`.
#'
#' @returns A single element of type character containing the automatically created ID of the step.
#'
#' @noRd
fun_create_step_id <- function(input_if,input_ifnot,input_then){
  # Add identifiers
  if (input_if != "") {
    if_string <- paste0("IF",input_if)
  } else {
    if_string <- input_if
  }

  if (input_ifnot != "") {
    ifnot_string <- paste0("IFNOT",input_ifnot)
  } else {
    ifnot_string <- input_ifnot
  }

  if (input_then != "") {
    then_string <- paste0("THEN",input_then)
  } else {
    then_string <- input_then
  }

  # Prepare out
  out <- paste0(if_string,ifnot_string,then_string)

  # Return
  return(out)
}

#' Create step descriptions
#'
#' Automatically creates the description for steps in the Steplist Creator `shiny` app.
#'
#' @param input_if A single element of type character. The description of the IF condition as provided in the first element of the output of
#'   `fun_create_step_if_desc()`.
#' @param input_ifnot A single element of type character. The description of the IFNOT condition as provided in the first element of the output of
#'   `fun_create_step_ifnot_desc()`.
#' @param input_then A single element of type character. The description of the THEN statement as provided in the output of
#' `fun_create_then_step_desc()`.
#' @param input_end_step A single element of type character. Indicator variable that describes if this step is at the end of a certain
#' sub-mechanism, e.g., symptom x occured. Its value comes from the corresponding checkbox and it will be "1", if it is an end step
#'
#' @returns A single element of type character containing the automatically created description of the step.
#'
#' @noRd
fun_create_step_desc <- function(input_if,input_ifnot,input_then,input_end_step){
  # Add identifiers
  if (input_if == "" & input_ifnot == "") {
    out <- input_then
  }

  if (input_if != "" & input_ifnot == "") {
    out <- paste("IF",input_if,"THEN",input_then)
  }

  if (input_if == "" & input_ifnot != "") {
    out <- paste("IFNOT",input_ifnot,"THEN",input_then)
  }

  if (input_if != "" & input_ifnot != "") {
    out <- paste("IF",input_if,"and IFNOT",input_ifnot,"THEN",input_then)
  }

  if (input_if == "") {
    out %<>% paste0("Start: ",.)
  }

  if (input_if != "" & input_end_step == "1") {
    out %<>% paste0("End: ",.)
  }

  # Return
  return(out)
}

#' Get module ID from dropdown
#'
#' Extracts the module ID from the dropdown selection for input into the STEP table. This function makes sure that correct values are provided on
#' start, and when nothing is selected.
#'
#' @param steplist_module Data.frame 'module' from the steplist.
#' @param input The value of the dropdown, i.e., input$step_input_module.
#'
#' @noRd
fun_get_module_id <- function(steplist_module, input) {

  if (is.null(input)) {
    out <- ""
  } else {
    if (input == "") {
      out <- ""
    } else {
      out <- steplist_module$id_module[steplist_module$key_module == input & !is.na(steplist_module$key_module)]
    }
  }

  return(out)
}

#' Prepare options for outcome definition
#'
#' Processes the steplist to present the descrciptions of the THEN parts of the steps that were marked by end_step.
#'
#' @param steplist The current steplist, i.e., steplist().
#'
#' @noRd
get_options_outc <- function(steplist) {

  temp <- steplist[["step"]] %>%
            dplyr::filter(.data$end_step == "1") %>%
            dplyr::select(dplyr::all_of("desc_step"))

  if (nrow(temp) > 0) {
    for (i in 1:nrow(temp)) {
      start_temp <- temp$desc_step[i] %>% stringr::str_locate("THEN") %>% magrittr::extract(1,"end") %>% magrittr::add(1)
      temp$desc_step[i] %<>% stringr::str_sub(start_temp, -1) %>% stringr::str_trim("both")
    }
    out <- temp$desc_step %>% magrittr::extract(temp$desc_step %>% stringr::str_to_lower() %>% order())
  } else {
    out <- ""
  }
  return(out)
}

#' Get outc ID
#'
#' Get ID of single part of outcome definition from the steplist.
#'
#' @param input The THEN part of a step description without keyword 'THEN'.
#' @param steplist_step Data.frame 'step' from the steplist.
#'
#' @noRd
get_id_outc <- function(input, steplist_step) {
  if (is.null(input)) {
    out <- ""
  } else {
    if (input == "") {
      out <- ""
    } else {
      temp <- steplist_step %>% dplyr::filter(.data$end_step == "1")
      pattern <- paste0(input,"$")
      if (temp$desc_step %>% stringr::str_detect(pattern) %>% sum() %>% magrittr::is_greater_than(0)) {
        idstep <- temp$id_step[temp$desc_step %>% stringr::str_detect(pattern)]
        start_temp <- idstep %>% stringr::str_locate("THEN") %>% magrittr::extract(1,"end") %>% magrittr::add(1)
        out <- idstep %>% stringr::str_sub(start_temp, -1) %>% stringr::str_trim("both")
      } else {
        out <- "NA"
      }
    }
  }
  return(out)
}

#' Combine outc selection
#'
#' Combining the inputs from the dropdown menus in tab OUTCOME.
#'
#' @param input A list of selected values from the dropdown menus in tab OUTCOME.
#' @param coll Boolean indicator describing if vector should be collapsed to on element.
#'
#' @noRd
create_outc_desc <- function(input, coll) {
  input %<>% unlist() %>% .[. != ""]

  if (coll) {
    if (length(input) == 0) {
      out <- ""
    } else {
      out <- input %>% stringr::str_c(collapse = " and ")
    }
    return(out)
  } else {
    if (length(input) == 0) {
      out <- ""
    } else {
      out <- input
    }
  }
  return(out)
}

check_outc_duplicates <- function(to_add_id_outc, steplist_part) {
  out <- FALSE

  if (steplist_part %>% nrow() %>% magrittr::is_greater_than(0)) {
    in_ids <- to_add_id_outc %>% stringr::str_split("\\+") %>% unlist()
    for (i in 1:nrow(steplist_part)) {

      temp_ids <- steplist_part$id_outc[i] %>% stringr::str_split("\\+") %>% unlist()

      if ((in_ids %>% magrittr::is_in(temp_ids) %>% sum() %>% magrittr::equals(length(in_ids))) &
          (temp_ids %>% magrittr::is_in(in_ids) %>% sum() %>% magrittr::equals(length(temp_ids)))) {
        out <- TRUE
      }
    }

  }

  return(out)
}
