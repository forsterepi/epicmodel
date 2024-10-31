#' DOES shinymodule UI
#'
#' Shinymodule UI for the DOES tab.
#'
#' @param id Namespace argument.
#'
#' @noRd
doesUI <- function(id) {
  ns <- NS(id)
  tabPanel("DOES",
           fluidRow(
             column(3,
                    add_text_input(NS(id,"input_key"),"key"),
                    add_text_does(NS(id,"input_singular"),
                                  NS(id,"input_plural"),
                                  NS(id,"input_nosubject")),
                    add_check_does(NS(id,"input_then_object")),
                    actionButton(NS(id,"add"), label = "Add DOES segment") %>%
                      prompter::add_prompt(position = "right",
                                           message = "Fill in all fields above and click the button to add a DOES segment. IDs are added automatically."),
                    br(),br(),
                    deleteUI(NS(id,"delete"))
             ),
             column(9,
                    tableUI(NS(id,"tbl"))
             )
           )
  )
}

#' DOES shinymodule server
#'
#' Shinymodule Server for the DOES tab.
#'
#' @param id Namespace argument.
#' @param steplist Steplist object, which contains the data as an
#'   `epicmodel_steplist` class object. This argument is reactive.
#'
#' @noRd
doesServer <- function(id, steplist) {
  stopifnot(is.reactive(steplist))
  steplist_part <- "does"

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
        updateTextInput(session, inputId = "input_singular", label = NULL, value = "")
        updateTextInput(session, inputId = "input_plural", label = NULL, value = "")
        updateTextInput(session, inputId = "input_nosubject", label = NULL, value = "")
        updateCheckboxInput(session ,inputId = "input_then_object", label = "THEN object?", value = F)
      }
      if ((input$input_key %>% stringr::str_trim("both") %>% stringr::str_to_lower() %>% magrittr::is_in(steplist_temp[[steplist_part]][[paste0("key_",steplist_part)]] %>% stringr::str_trim("both") %>% stringr::str_to_lower()) %>% magrittr::not()) &
          input$input_key %>% stringr::str_trim("both") %>% magrittr::equals("") %>% magrittr::not()) {
        to_add <- data.frame(id_does = fun_get_id(steplist_temp[[steplist_part]][[paste0("id_",steplist_part)]],"does"),
                             key_does = input$input_key %>% stringr::str_trim("both"),
                             subject_singular_does = input$input_singular %>% stringr::str_trim("both"),
                             subject_plural_does = input$input_plural %>% stringr::str_trim("both"),
                             no_subject_does = input$input_nosubject %>% stringr::str_trim("both"),
                             then_object_does = as.character(as.numeric(input$input_then_object)))

        steplist_temp[[steplist_part]] <- rbind(steplist_temp[[steplist_part]],to_add)
        steplist(steplist_temp)

        updateTextInput(session, inputId = "input_key", label = NULL, value = "")
        updateTextInput(session, inputId = "input_singular", label = NULL, value = "")
        updateTextInput(session, inputId = "input_plural", label = NULL, value = "")
        updateTextInput(session, inputId = "input_nosubject", label = NULL, value = "")
        updateCheckboxInput(session ,inputId = "input_then_object", label = "THEN object?", value = F)
      }
    })
  })
}

#' DOES shinymodule demo app
#'
#' Demo app for the shinymodule of the DOES tab, used for testing only.
#'
#' @noRd
doesApp <- function() {
  ui <- tagList(
    prompter::use_prompt(),
    navbarPage(
      title = "Create Steps for SCC Models",
      doesUI("does_tab")
    )
  )

  server <- function(input, output, session) {
    steplist <- reactiveVal(empty_steplist())

    doesServer("does_tab", steplist)

  }

  shinyApp(ui, server)
}
