#' WHAT shinymodule UI
#'
#' Shinymodule UI for the WHAT tab.
#'
#' @param id Namespace argument.
#'
#' @noRd
whatUI <- function(id) {
  ns <- NS(id)
  tabPanel("WHAT",
           fluidRow(
             column(3,
                    add_text_input(NS(id,"input_key"),"key"),
                    add_text_input(NS(id,"input_desc"),"desc"),
                    add_check_what(NS(id,"input_plural")),
                    actionButton(NS(id,"add"), label = "Add WHAT segment") %>%
                      prompter::add_prompt(position = "right",
                                           message = "Fill in all fields above and click the button to add a WHAT segment. IDs are added automatically."),
                    br(),br(),
                    deleteUI(NS(id,"delete"))
             ),
             column(9,
                    tableUI(NS(id,"tbl"))
             )
            )
          )
}

#' WHAT shinymodule server
#'
#' Shinymodule Server for the WHAT tab.
#'
#' @param id Namespace argument.
#' @param steplist Steplist object, which contains the data as an
#'   `epicmodel_steplist` class object. This argument is reactive.
#'
#' @noRd
whatServer <- function(id, steplist) {
  stopifnot(is.reactive(steplist))
  steplist_part <- "what"

  # moduleServer
  moduleServer(id, function(input, output, session) {

    tableServer("tbl", steplist, steplist_part,
                editable = list(target = "cell", disable = list(columns = c(0))),
                options = list(scrollY = '750px', scrollCollapse = TRUE,
                               paging = FALSE, searchHighlight = TRUE))

    deleteServer("delete", steplist, steplist_part)

    observeEvent(input$add, {
      steplist_temp <- steplist()

      if (input$input_key %>% stringr::str_trim("both") %>% magrittr::equals("")) {
        shinyalert::shinyalert(title = "Warning!", text = "The keyword is empty.", type = "warning")
      }
      if (input$input_key %>% stringr::str_trim("both") %>% stringr::str_to_lower() %>% magrittr::is_in(steplist_temp[[steplist_part]][[paste0("key_",steplist_part)]] %>% stringr::str_trim("both") %>% stringr::str_to_lower())) {
        shinyalert::shinyalert(title = "Warning!", text = "This WHAT segment already exists.", type = "warning")
        updateTextInput(session, inputId = "input_key", label = NULL, value = "")
        updateTextInput(session, inputId = "input_desc", label = NULL, value = "")
      }
      if ((input$input_key %>% stringr::str_trim("both") %>% stringr::str_to_lower() %>% magrittr::is_in(steplist_temp[[steplist_part]][[paste0("key_",steplist_part)]] %>% stringr::str_trim("both") %>% stringr::str_to_lower()) %>% magrittr::not()) &
          input$input_key %>% stringr::str_trim("both") %>% magrittr::equals("") %>% magrittr::not()) {
        to_add <- data.frame(id_what = fun_get_id(steplist_temp[[steplist_part]][[paste0("id_",steplist_part)]],"what"),
                             key_what = input$input_key %>% stringr::str_trim("both"),
                             desc_what = input$input_desc %>% stringr::str_trim("both"),
                             plural_what = as.character(as.numeric(input$input_plural)))

        steplist_temp[[steplist_part]] <- rbind(steplist_temp[[steplist_part]],to_add)
        steplist(steplist_temp)

        updateTextInput(session, inputId = "input_key", label = NULL, value = "")
        updateTextInput(session, inputId = "input_desc", label = NULL, value = "")
      }
    })
  })

}

#' WHAT shinymodule demo app
#'
#' Demo app for the shinymodule of the WHAT tab, used for testing only.
#'
#' @noRd
whatApp <- function() {
  ui <- tagList(
    prompter::use_prompt(),
    navbarPage(
      title = "Create Steps for SCC Models",
      whatUI("what_tab")
    )
  )

  server <- function(input, output, session) {
    steplist <- reactiveVal(empty_steplist())

    whatServer("what_tab", steplist)

  }

  shinyApp(ui, server)
}
