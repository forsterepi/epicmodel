#' All TRUE?
#'
#' Does a vector of TRUE/FALSE only contain TRUE?
#'
#' @param x A vector of TRUE/FALSE of length > 0, without missings. NULL is not allowed.
#'
#' @returns TRUE or FALSE
#'
#' @noRd
all_true <- function(x) {
  # Check input
  rlang::try_fetch({
      checkmate::assert_logical(x, any.missing = F, min.len = 1, null.ok = F)
    },error = function(cnd) {cli::cli_abort(c("Input validation error",
                                              "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                            parent = cnd, class = "input_all_true")
  })
  #=============================================================================
  out <- x %>% magrittr::not() %>% sum() %>% magrittr::equals(0)
  #=============================================================================
  #Check output
  rlang::try_fetch({
      checkmate::assert_logical(out, any.missing = F, len = 1, null.ok = F)
    },error = function(cnd) {cli::cli_abort(c("Output validation error",
                                              "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                            parent = cnd, class = "output_all_true")
  })
  #=============================================================================
  return(out)
}

#' All FALSE?
#'
#' Does a vector of TRUE/FALSE only contain FALSE?
#'
#' @param x A vector of TRUE/FALSE of length > 0, without missings. NULL is not allowed.
#'
#' @returns TRUE or FALSE
#'
#' @noRd
all_false <- function(x) {
  # Check input
  rlang::try_fetch({
      checkmate::assert_logical(x, any.missing = F, min.len = 1, null.ok = F)
    },error = function(cnd) {cli::cli_abort(c("Input validation error",
                                              "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                            parent = cnd, class = "input_all_false")
  })
  #=============================================================================
  out <- x %>% sum() %>% magrittr::equals(0)
  #=============================================================================
  #Check output
  rlang::try_fetch({
      checkmate::assert_logical(out, any.missing = F, len = 1, null.ok = F)
    },error = function(cnd) {cli::cli_abort(c("Output validation error",
                                              "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                            parent = cnd, class = "output_all_false")
  })
  #=============================================================================
  return(out)
}
