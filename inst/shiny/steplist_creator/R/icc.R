#' ICC shinymodule UI
#'
#' Shinymodule UI for the ICC tab.
#'
#' @param id Namespace argument.
#'
#' @noRd
iccUI <- function(id) {
  ns <- NS(id)
  tabPanel("ICC",
           fluidRow(
             column(3,
                    wellPanel(h4("New incompatible component causes") %>%
                                prompter::add_prompt(position = "right",
                                                     message = "Add two component causes that logically cannot be in the same suffcient cause."),
                              uiOutput(NS(id,"select_desc1")),
                              uiOutput(NS(id,"select_desc2")),
                              style = "padding: 10px; background: #69d3bf"),
                    actionButton(NS(id,"add"), label = "Add ICC pair") %>%
                      prompter::add_prompt(position = "right",
                                           message = "Fill in both fields and click the button to add a ICC pair. IDs are added automatically."),
                    br(),br(),
                    deleteUI(NS(id,"delete"))
             ),
             column(9,
                    tableUI(NS(id,"tbl"))
             )
           )
  )
}

#' ICC shinymodule server
#'
#' Shinymodule Server for the ICC tab.
#'
#' @param id Namespace argument.
#' @param steplist Steplist object, which contains the data as an
#'   `epicmodel_steplist` class object. This argument is reactive.
#'
#' @noRd
iccServer <- function(id, steplist) {
  stopifnot(is.reactive(steplist))

  # moduleServer
  moduleServer(id, function(input, output, session) {

    output$select_desc1 <- renderUI(selectInput(NS(id,"select_desc1"), "First Component Cause", selectize = T,
                                                  c(Choose = "", steplist()[["step"]]$desc_step %>%
                                                      magrittr::extract(stringr::str_detect(.,"^Start:")) %>%
                                                      magrittr::extract(order(.)))))

    output$select_desc2 <- renderUI(selectInput(NS(id,"select_desc2"), "Second Component Cause", selectize = T,
                                                c(Choose = "", steplist()[["step"]]$desc_step %>%
                                                    magrittr::extract(stringr::str_detect(.,"^Start:")) %>%
                                                    magrittr::extract(order(.)))))

    tableServer("tbl", steplist, "icc",
                editable = FALSE,
                options = list(scrollY = '750px', scrollCollapse = TRUE,
                               paging = FALSE, searchHighlight = TRUE))

    deleteServer("delete", steplist, "icc")

    observeEvent(input$add, {
      steplist_temp <- steplist()

      icc_empty <- F
      if (input$select_desc1 %>% magrittr::equals("") | input$select_desc2 %>% magrittr::equals("")) {
        icc_empty <- T
      }

      if (icc_empty) {
        shinyalert::shinyalert(title = "Warning!", text = "Please select two component causes.", type = "warning")
      } else {

        id1_input <- steplist_temp[["step"]]$id_step[steplist_temp[["step"]]$desc_step == input$select_desc1 & !is.na(steplist_temp[["step"]]$desc_step)]
        id2_input <- steplist_temp[["step"]]$id_step[steplist_temp[["step"]]$desc_step == input$select_desc2 & !is.na(steplist_temp[["step"]]$desc_step)]

        icc_equal <- F
        if (id1_input == id2_input) {
          icc_equal <- T
        }

        if (icc_equal) {
          shinyalert::shinyalert(title = "Warning!", text = "Please select two different component causes.", type = "warning")
        } else {

          icc_exists <- F

          exists_part1 <- paste0(steplist_temp[["icc"]]$id1,steplist_temp[["icc"]]$id2)
          exists_part2 <- paste0(steplist_temp[["icc"]]$id2,steplist_temp[["icc"]]$id1)

          if (paste0(id1_input,id2_input) %>% magrittr::is_in(c(exists_part1,exists_part2))) {
            icc_exists <- T
          }

          if (icc_exists) {
            shinyalert::shinyalert(title = "Warning!", text = "This icc pair already exists.", type = "warning")
            updateSelectInput(session, inputId = "select_desc1", "First Component Cause", selected = "")
            updateSelectInput(session, inputId = "select_desc2", "Second Component Cause", selected = "")
          }

          if ((!icc_empty) & (!icc_exists) & (!icc_equal)) {
            to_add <- data.frame(id_icc = fun_get_id(steplist_temp[["icc"]]$id_icc,"icc"),
                                 id1 = id1_input,
                                 id2 = id2_input,
                                 desc1 = input$select_desc1,
                                 desc2 = input$select_desc2)

            steplist_temp[["icc"]] <- rbind(steplist_temp[["icc"]],to_add)
            steplist(steplist_temp)

            updateSelectInput(session, inputId = "select_desc1", "First Component Cause", selected = "")
            updateSelectInput(session, inputId = "select_desc2", "Second Component Cause", selected = "")
          }
        }
      }
    })

  })
}

#' ICC shinymodule demo app
#'
#' Demo app for the shinymodule of the ICC tab, used for testing only.
#'
#' @noRd
iccApp <- function() {
  ui <- tagList(
    prompter::use_prompt(),
    navbarPage(
      title = "Create Steps for SCC Models",
      iccUI("icc_tab")
    )
  )

  server <- function(input, output, session) {
    steplist <- reactiveVal(empty_steplist())

    iccServer("icc_tab", steplist)

  }

  shinyApp(ui, server)
}
