#' Launch steplist creator `shiny` app
#'
#' Run this function to start the Steplist Creator `shiny` app.
#'
#' @returns The `launch_steplist_creator` function is used for the side effect of starting the Steplist Creator `shiny` app.
#' @export
#'
#' @examples
#' if(interactive()){
#' launch_steplist_creator()
#' }
launch_steplist_creator <- function() {
  appDir <- system.file("shiny", "steplist_creator", package = "epicmodel")
  if (appDir == "") {
    cli::cli_abort("Could not find directory. Try re-installing `epicmodel`.")
  }

  #shiny::runApp(appDir, display.mode = "normal")
  shiny::shinyAppDir(appDir)
}
