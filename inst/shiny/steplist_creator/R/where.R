#' WHERE shinymodule UI
#'
#' Shinymodule UI for the WHERE tab.
#'
#' @param id Namespace argument.
#'
#' @noRd
whereUI <- function(id) {
  ns <- NS(id)
  tabPanel("WHERE",
           fluidRow(
             column(3,
                    add_text_input(NS(id,"input_key"),"key"),
                    add_text_input(NS(id,"input_desc"),"desc"),
                    actionButton(NS(id,"add"), label = "Add WHERE segment") %>%
                      prompter::add_prompt(position = "right",
                                           message = "Fill in all fields above and click the button to add a WHERE segment. IDs are added automatically."),
                    br(),br(),
                    deleteUI(NS(id,"delete"))
             ),
             column(9,
                    tableUI(NS(id,"tbl"))
             )
           )
  )
}

#' WHERE shinymodule server
#'
#' Shinymodule Server for the WHERE tab.
#'
#' @param id Namespace argument.
#' @param steplist Steplist object, which contains the data as an
#'   `epicmodel_steplist` class object. This argument is reactive.
#'
#' @noRd
whereServer <- function(id, steplist) {
  stopifnot(is.reactive(steplist))
  steplist_part <- "where"

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
        shinyalert::shinyalert(title = "Warning!", text = "This where segment already exists.", type = "warning")
        updateTextInput(session, inputId = "input_key", label = NULL, value = "")
        updateTextInput(session, inputId = "input_desc", label = NULL, value = "")
      }
      if ((input$input_key %>% stringr::str_trim("both") %>% stringr::str_to_lower() %>% magrittr::is_in(steplist_temp[[steplist_part]][[paste0("key_",steplist_part)]] %>% stringr::str_trim("both") %>% stringr::str_to_lower()) %>% magrittr::not()) &
          input$input_key %>% stringr::str_trim("both") %>% magrittr::equals("") %>% magrittr::not()) {
        to_add <- data.frame(id_where = fun_get_id(steplist_temp[[steplist_part]][[paste0("id_",steplist_part)]],"where"),
                             key_where = input$input_key %>% stringr::str_trim("both"),
                             desc_where = input$input_desc %>% stringr::str_trim("both"))

        steplist_temp[[steplist_part]] <- rbind(steplist_temp[[steplist_part]],to_add)
        steplist(steplist_temp)

        updateTextInput(session, inputId = "input_key", label = NULL, value = "")
        updateTextInput(session, inputId = "input_desc", label = NULL, value = "")
      }
    })
  })
}

#' WHERE shinymodule demo app
#'
#' Demo app for the shinymodule of the WHERE tab, used for testing only.
#'
#' @noRd
whereApp <- function() {
  ui <- tagList(
    prompter::use_prompt(),
    navbarPage(
      title = "Create Steps for SCC Models",
      whereUI("where_tab")
    )
  )

  server <- function(input, output, session) {
    steplist <- reactiveVal(empty_steplist())

    whereServer("where_tab", steplist)

  }

  shinyApp(ui, server)
}
