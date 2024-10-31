#' Delete shinymodule UI
#'
#' Shinymodule UI for the delete actionButton.
#'
#' @param id Namespace argument.
#'
#' @noRd
deleteUI <- function(id) {
  ns <- NS(id)
  actionButton(NS(id,"delete"), label = "Delete row") %>%
    prompter::add_prompt(position = "right", message = "Delete a row from the table by specifying the corresponding ID in the pop-up window.")
}

#' Delete shinymodule server
#'
#' Shinymodule Server for the delete actionButton.
#'
#' @param id Namespace argument.
#' @param steplist Steplist object, which contains the data as an
#'   `epicmodel_steplist` class object. This argument is reactive.
#' @param steplist_part Addresses a specific data.frame element of steplist.
#'   Options are element names of class `epicmodel_steplist`. This argument is
#'   not reactive.
#'
#' @noRd
deleteServer <- function(id, steplist, steplist_part) {
  stopifnot(is.reactive(steplist))
  stopifnot(!is.reactive(steplist_part))

  moduleServer(id, function(input, output, session) {
    observeEvent(input$delete, {
      shinyalert::shinyalert(title = "Delete row", type = "input", inputType = "text",
                             inputPlaceholder = paste0("Provide id_",steplist_part),
                 callbackR = function(delete_id) {
                   steplist_temp <- steplist()
                   steplist_temp[[steplist_part]] %<>% dplyr::filter(.data[[paste0("id_",steplist_part)]] != delete_id)
                   steplist(steplist_temp)}
      )
    })
  })
}

#' Delete shinymodule demo app
#'
#' Demo app for shinymodule delete, used for testing only.
#'
#' @noRd
deleteApp <- function() {

  ui <- fluidPage(
    prompter::use_prompt(),
    tableUI("does_tbl"),
    deleteUI("does_delete")
  )

  server <- function(input, output, session) {
    steplist <- reactiveVal(empty_steplist())

    tableServer("does_tbl", steplist, "does",
                editable = list(target = "cell", disable = list(columns = c(0))),
                options = list(pageLength = 20, searchHighlight = TRUE))

    deleteServer("does_delete", steplist, "does")
  }

  shinyApp(ui, server)
}
