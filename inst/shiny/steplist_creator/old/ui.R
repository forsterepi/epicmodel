#' UI of Steplist Creator `shiny` app
#'
#' The UI of Steplist Creator `shiny` app including UI modules and options.
#'
#' @noRd
ui <- tagList(
  shinyjs::useShinyjs(),
  prompter::use_prompt(),
  tags$style(type = "text/css", ".form-control.shiny-bound-input, .numeric-input {height: 35px;}"),
  navbarPage(
    theme = shinythemes::shinytheme("flatly"),
    title = "Create Steplists for SCC Models",
    ui_start_tab(),
    whatUI("what_tab"),
    doesUI("does_tab"),
    whereUI("where_tab"),
    moduleTabUI("module_tab"),
    ui_step_tab(),
    iccUI("icc_tab"),
    ui_outc_tab()
  )
)
