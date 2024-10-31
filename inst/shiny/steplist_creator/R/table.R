#' Table shinymodule UI
#'
#' Shinymodule UI for tables using the DT package.
#'
#' @param id Namespace argument.
#'
#' @noRd
tableUI <- function(id) {
  ns <- NS(id)
  DT::DTOutput(NS(id,"tbl"))
}

#' Table shinymodule server
#'
#' Shinymodule Server for tables using the DT package. It contains displaying
#' and editing tables.
#'
#' @param id Namespace argument.
#' @param steplist Steplist object, which contains the data as an
#'   `epicmodel_steplist` class object. This argument is reactive.
#' @param steplist_part Addresses a specific data.frame element of steplist.
#'   Options are element names of class `epicmodel_steplist`. This argument is
#'   not reactive.
#' @param editable A list of arguments defining how the table is editable. This
#'   argument is not reactive.
#' @param options A list of arguments defining additional options of the table.
#'   This argument is not reactive.
#'
#' @noRd
tableServer <- function(id, steplist, steplist_part, editable, options) {
  stopifnot(is.reactive(steplist))
  stopifnot(!is.reactive(steplist_part))
  stopifnot(!is.reactive(editable))
  stopifnot(!is.reactive(options))

  moduleServer(id, function(input, output, session) {
    output$tbl <- DT::renderDataTable(steplist()[[steplist_part]], server = T, rownames = F, selection = "none",
                                      editable = editable, options = options)

    observeEvent(input$tbl_cell_edit, {
      steplist_temp <- steplist()
      steplist_temp[[steplist_part]] <- DT::editData(steplist_temp[[steplist_part]], input$tbl_cell_edit, 'tbl', rownames = F)
      steplist(steplist_temp)
    })
  })
}

#' Table shinymodule demo app
#'
#' Demo app for shinymodule table, used for testing only.
#'
#' @noRd
tableApp <- function() {
  ui <- fluidPage(
    tableUI("does_tbl")
  )

  server <- function(input, output, session) {
    steplist <- reactiveVal(empty_steplist())
    tableServer("does_tbl", steplist, "does",
                editable = list(target = "cell", disable = list(columns = c(0))),
                options = list(pageLength = 20, searchHighlight = TRUE))
  }

  shinyApp(ui, server)
}
