#' MODULE shinymodule UI
#'
#' Shinymodule UI for the MODULE tab. The function is called `moduleTabUI()` to
#' be consistent with `moduleTabServer()`, which had to be named like that
#' because `moduleServer()` already exists and plays an essential role in
#' defining shinymodules.
#'
#' @param id Namespace argument.
#'
#' @noRd
moduleTabUI <- function(id) {
  ns <- NS(id)
  tabPanel("MODULE",
           fluidRow(
             column(3,
                    add_text_input(NS(id,"input_key"),"key"),
                    add_text_input(NS(id,"input_desc"),"desc_module"),
                    actionButton(NS(id,"add"), label = "Add Module") %>%
                      prompter::add_prompt(position = "right",
                                           message = "Fill in all fields above and click the button to add a module. IDs are added automatically."),
                    br(),br(),
                    deleteUI(NS(id,"delete"))
             ),
             column(9,
                    tableUI(NS(id,"tbl"))
             )
           )
  )
}

#' MODULE shinymodule server
#'
#' Shinymodule Server for the MODULE tab. The function is called
#' `moduleTabServer()`, because `moduleServer()` already exists and plays an
#' essential role in defining shinymodules.
#'
#' @param id Namespace argument.
#' @param steplist Steplist object, which contains the data as an
#'   `epicmodel_steplist` class object. This argument is reactive.
#'
#' @noRd
moduleTabServer <- function(id, steplist) {
  stopifnot(is.reactive(steplist))
  steplist_part <- "module"

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
        shinyalert::shinyalert(title = "Warning!", text = "This module segment already exists.", type = "warning")
        updateTextInput(session, inputId = "input_key", label = NULL, value = "")
        updateTextInput(session, inputId = "input_desc", label = NULL, value = "")
      }
      if ((input$input_key %>% stringr::str_trim("both") %>% stringr::str_to_lower() %>% magrittr::is_in(steplist_temp[[steplist_part]][[paste0("key_",steplist_part)]] %>% stringr::str_trim("both") %>% stringr::str_to_lower()) %>% magrittr::not()) &
          input$input_key %>% stringr::str_trim("both") %>% magrittr::equals("") %>% magrittr::not()) {
        to_add <- data.frame(id_module = fun_get_id(steplist_temp[[steplist_part]][[paste0("id_",steplist_part)]],"module"),
                             key_module = input$input_key %>% stringr::str_trim("both"),
                             desc_module = input$input_desc %>% stringr::str_trim("both"))

        steplist_temp[[steplist_part]] <- rbind(steplist_temp[[steplist_part]],to_add)
        steplist(steplist_temp)

        updateTextInput(session, inputId = "input_key", label = NULL, value = "")
        updateTextInput(session, inputId = "input_desc", label = NULL, value = "")
      }
    })
  })
}

#' MODULE shinymodule demo app
#'
#' Demo app for the shinymodule of the MODULE tab, used for testing only.
#'
#' @noRd
moduleApp <- function() {
  ui <- tagList(
    prompter::use_prompt(),
    navbarPage(
      title = "Create Steps for SCC Models",
      moduleTabUI("module_tab")
    )
  )

  server <- function(input, output, session) {
    steplist <- reactiveVal(empty_steplist())

    moduleTabServer("module_tab", steplist)

  }

  shinyApp(ui, server)
}
