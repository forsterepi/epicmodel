#' UI start tab
#'
#' Defines the UI elements of tab 'Start' in the Steplist Creator `shiny` app.
#'
#' @noRd
ui_start_tab <- function(){
  tabPanel("START",
           h2("Welcome to the Steplist Creator Shiny App in the epicmodel package!"),
           br(),
           column(6,
              p("This app helps you to create steplists, which are used to create sufficient-component cause (SCC) models.
               You can just start with an empty list or you can load an existing steplist that you previously downloaded."),
              p("If you need help, the epicmodel homepage contains articles on steplists in general and on how to use this app."),
              p("When you are done, do not forget to download your list! Otherwise your progress will be lost.")
           ),
           column(6,
             wellPanel(
              h4("Upload Steplist"),
              fileInput("upload", label = NULL, buttonLabel = "Upload Steplist",
                        accept = ".rds", placeholder = "Select steplist object (.rds)"),
              h4("Download Steplist"),
              downloadButton("download", label = "Download Steplist"),
              style = "padding: 10px; background: #69d3bf"
            )
        )
  )
}

#' UI step tab
#'
#' Defines the UI elements of tab 'STEP' in the Steplist Creator `shiny` app.
#'
#' @noRd
ui_step_tab <- function(){
  empty <- empty_steplist()
  initial_then_data <- empty$then

  tabPanel("STEP",
           fluidRow(
             column(7,
                    fluidRow(
                      column(6,
                             add_if_ifnot_header("if"),
                             add_if_ifnot_buttons("more_lines_if","less_lines_if"),
                             wellPanel(
                               div(id = "if_placeholder",
                                   div(
                                     id = "if_row1",
                                     class = "input-row",
                                     fluidRow(
                                       column(2, numericInput("if_numeric_input1", label = NULL, value = 1, min = 1)),
                                       column(10, selectInput("if_select_input1", label = NULL,
                                                              choices = c(Choose = "", initial_then_data$desc_then %>% magrittr::extract(order(.)))))
                                     )
                                   )),
                               style = "padding: 10px; background: #69d3bf"
                             ),
                             add_if_ifnot_preview("step_if_id", "step_if_desc", "IF")
                      ),
                      column(6,
                             add_if_ifnot_header("ifnot"),
                             add_if_ifnot_buttons("more_lines_ifnot","less_lines_ifnot"),
                             wellPanel(
                               div(id = "ifnot_placeholder",
                                   div(
                                     id = "ifnot_row1",
                                     class = "input-row",
                                     fluidRow(
                                       column(2, numericInput("ifnot_numeric_input1", label = NULL, value = 1, min = 1)),
                                       column(10, selectInput("ifnot_select_input1", label = NULL,
                                                              choices = c(Choose = "", initial_then_data$desc_then %>% magrittr::extract(order(.)))))
                                     )
                                   )),
                               style = "padding: 10px; background: #69d3bf"
                             ),
                             add_if_ifnot_preview("step_ifnot_id", "step_ifnot_desc", "IFNOT")
                      )
                    ),
                    DT::DTOutput("step_tbl")
             ),
             column(2,
                    h4("Create THEN Statement"),
                    wellPanel(
                      uiOutput("select_subject"),
                      uiOutput("select_does"),
                      uiOutput("select_object"),
                      uiOutput("select_where"),
                      style = "padding: 10px; background: #69d3bf"),
                    h4("Additional STEP Information"),
                    wellPanel(
                      checkboxInput("step_input_end", label = "End step?", value = F),
                      uiOutput("step_input_module"),
                      textAreaInput("step_input_ref", label = "References", placeholder = "Author et al. 2024",
                                    height = "200%", resize = "vertical"),
                      textAreaInput("step_input_note", label = "Notes", placeholder = "Important information and conflicting findings",
                                    height = "200%", resize = "vertical"),
                      style = "padding: 10px; background: #69d3bf"
                    ),
                    wellPanel(
                      h5("Actions: STEP"),
                      actionButton("step_add", label = "Add") %>%
                        prompter::add_prompt(position = "left", message = "Click to add a STEP. Also adds the THEN statement to the THEN table if it's not
                                 already available."),
                      actionButton("step_delete", label = "Delete") %>%
                        prompter::add_prompt(position = "left", message = "Delete a row from the STEP table by specifying the corresponding ID in the pop-up window."),
                      actionButton("step_clear", label = "Clear") %>%
                        prompter::add_prompt(position = "left", message = "Click to revert all inputs to their empty default states."),
                      style = "padding: 0px; background: #ffffff"
                    )
             ),
             column(3,
                    add_then_preview("step_then_id","step_then_desc"),
                    wellPanel(
                      h5("Actions: THEN"),
                      actionButton("then_add", label = "Add") %>%
                        prompter::add_prompt(position = "left", message = "Select Subject, DOES, Object, and WHERE (or some of them) and click the button
                                  for the THEN statment to be selectable for IF and IFNOT conditions."),
                      actionButton("then_delete", label = "Delete") %>%
                        prompter::add_prompt(position = "left", message = "Delete a row from the THEN table by specifying the corresponding ID in the pop-up window."),
                      style = "padding: 0px; background: #ffffff"
                    ),
                    DT::DTOutput("then_tbl"))
           )
  )
}

#' UI outcome definition tab
#'
#' Defines the UI elements of tab 'OUTCOME' in the Steplist Creator `shiny` app.
#'
#' @noRd
ui_outc_tab <- function() {
  empty <- empty_steplist()
  initial_step_data <- empty$step

  tabPanel("OUTCOME",
           fluidRow(
             column(4,
                    h2("Create Outcome Definition"),
                    add_if_ifnot_buttons("more_lines_outc","less_lines_outc"),
                    wellPanel(
                     div(id = "outc_placeholder",
                         div(
                           id = "outc_row1",
                           class = "input-row",
                           selectInput("outc_select_input1", label = NULL,
                                                    choices = c(Choose = "", initial_step_data$desc_step %>%
                                                                  magrittr::extract(order(.))))
                         )),
                     style = "padding: 10px; background: #69d3bf"
                    ),
                    wellPanel(
                      h5("Actions: OUTCOME"),
                      actionButton("outc_add", label = "Add") %>%
                        prompter::add_prompt(position = "right", message = "Click to add an OUTCOME definition."),
                      actionButton("outc_delete", label = "Delete") %>%
                        prompter::add_prompt(position = "right", message = "Delete a row from the OUTCOME table by specifying the corresponding ID in the pop-up window."),
                      actionButton("outc_clear", label = "Clear") %>%
                        prompter::add_prompt(position = "right", message = "Click to revert all inputs to their empty default states."),
                      style = "padding: 0px; background: #ffffff"
                    )
             ),
             column(8,
                    DT::DTOutput("outc_tbl")
             )
           )
  )
}

#' Add text inputs to `shiny` UI
#'
#' Used to add keyword inputs on tabs WHAT, DOES, WHERE, and MODULE as well as
#' description inputs on tabs WHAT, WHERE, and MODULE.
#'
#' @param id The id of the textInput element.
#' @param type Indicates if the text input is for a keyword ("key") or a
#'   description ("desc"). The description input for tab MODULE has a special
#'   input ("desc_module").
#'
#' @noRd
add_text_input <- function(id, type) {

  long <- switch(type,
                 "key" = "keyword",
                 "desc" = "description",
                 "desc_module" = "description",
                 "Add long version for this type!"
  )
  message <- switch(type,
                    "key" = "Keywords are short descriptions.",
                    "desc" = "Descriptions are longer than keywords and contain the text that will appear in the STEP descriptions.",
                    "desc_module" = "Descriptions are longer than keywords.",
                    "Add message for this type!"
  )
  placeholder <- switch(type,
                        "key" = "Short keyword",
                        "desc" = "Complete description",
                        "desc_module" = "Complete description",
                        "Add placeholder for this type!"
  )

  wellPanel(h4(paste0("New ",long)) %>%
              prompter::add_prompt(position = "right", message = message),
            textInput(id, label = NULL, placeholder = placeholder),
            style = "padding: 10px; background: #69d3bf")
}

#' Add DOES description text inputs to `shiny` UI
#'
#' Used to add description inputs on tab DOES.
#'
#' @param id1 The id of the textInput element for the description for subjects
#'   in singular.
#' @param id2 The id of the textInput element for the description for subjects
#'   in plural.
#' @param id3 The id of the textInput element for the description for missing
#'   subjects.
#'
#' @noRd
add_text_does <- function(id1,id2,id3) {
  wellPanel(h4("New DOES variations") %>%
              prompter::add_prompt(position = "right", message = "In order to have a grammatically correct STEP description, add
                                            the corresponding variations: for WHAT segments in singular, for WHAT segments in plural, and if no
                                            subject (i.e., the WHAT segment before the DOES segment) is provided."),
            textInput(id1, label = NULL, placeholder = "For subjects in singular"),
            textInput(id2, label = NULL, placeholder = "For subjects in plural"),
            textInput(id3, label = NULL, placeholder = "For missing subjects"),
            style = "padding: 10px; background: #69d3bf")
}

#' Add WHAT checkbox input to `shiny` UI
#'
#' Used to add the checkbox input on tab WHAT, which indicates if it is singular
#' or plural.
#'
#' @param id The id of the checkboxInput element.
#'
#' @noRd
add_check_what <- function(id) {
  wellPanel(h4("Is the segment in singular or plural?") %>%
              prompter::add_prompt(position = "right",
                                   message = "This will affect the form of the DOES segment that is used in the step description."),
            checkboxInput(id, label = "Plural?", value = F),
            style = "padding: 10px; background: #69d3bf")
}

#' Add DOES checkbox input to `shiny` UI
#'
#' Used to add the checkbox input on tab DOES, which indicates if it needs a
#' THEN object.
#'
#' @param id The id of the checkboxInput element.
#'
#' @noRd
add_check_does <- function(id) {
  wellPanel(h4("Does this DOES segment need a THEN object?") %>%
              prompter::add_prompt(position = "right", message = "Some DOES segments, e.g., inhibition, need THEN instead of WHAT objects."),
            checkboxInput(id, label = "THEN object?", value = F),
            style = "padding: 10px; background: #69d3bf")
}

#' Add IF/IFNOT buttons to `shiny` UI
#'
#' Used to add two buttons to the IF or IFNOT section of the STEP tab. One
#' button adds a new row and the other removes the last row.
#'
#' @param id1 The id of the actionButton, which adds a row.
#' @param id2 The id of the actionButton, which removes a row.
#'
#' @noRd
add_if_ifnot_buttons <- function(id1,id2) {
  wellPanel(
    actionButton(id1, "Add statement") %>%
      prompter::add_prompt(position = "right", message = "Adds new line."),
    actionButton(id2, "Remove statement") %>%
      prompter::add_prompt(position = "right", message = "Deletes last line. If only one line is left, clicking the button clears the selection."),
    style = "padding: 0px; background: #ffffff"
  )
}

#' Add IF/IFNOT previews to `shiny` UI
#'
#' Used to add two textOutputs to the IF or IFNOT section of the STEP tab,
#' previewing the automatically created ID and Description of the corresponding
#' IF/IFNOT condition.
#'
#' @param id1 The id of the textOutput, which previews the ID.
#' @param id2 The id of the textOutput, which previews the description.
#' @param type Either "IF" or "IFNOT", adjusting the text elements surrounding the textOutput elements.
#'
#' @noRd
add_if_ifnot_preview <- function(id1, id2, type) {
  tagList(
    h5(paste0(type," ID")) %>%
      prompter::add_prompt(position = "right", message = paste0("Previews the ID combination for the ",type," condition.")),
    wellPanel(textOutput(id1), style = "padding: 5px; background: #ffffff; border-color: #95a5a6"),
    h5(paste0(type," Description")) %>%
      prompter::add_prompt(position = "right", message = paste0("Previews the combined description for the ",type," condition.")),
    wellPanel(textOutput(id2), style = "padding: 5px; background: #ffffff; border-color: #95a5a6")
  )
}

#' Add IF/IFNOT header to `shiny` UI
#'
#' Used to add the header to the IF or IFNOT section of the STEP tab.
#'
#' @param type Either "if" or "ifnot", selecting the corresponding variant.
#'
#' @noRd
add_if_ifnot_header <- function(type = c("if","ifnot")) {
  type <- match.arg(type)

  if (type == "if") {
    out <- tagList(
      h4("Create IF Condition") %>%
        prompter::add_prompt(position = "right", message = "The IF condition is a collection of available, previously created THEN statments,
                                       which together must be fulfilled, in order for the STEP to occur.")
    )
  }

  if (type == "ifnot") {
    out <- tagList(
      h4("Create IFNOT Condition") %>%
        prompter::add_prompt(position = "right", message = "The IFNOT condition is a collection of available, previously created THEN statments,
                                       which together must not be fulfilled, in order for the STEP to occur.")
    )
  }

  return(out)
}

#' Add THEN previews to `shiny` UI
#'
#' Used to add two textOutputs to the THEN section of the STEP tab,
#' previewing the automatically created ID and Description of the corresponding
#' THEN statement.
#'
#' @param id1 The id of the textOutput, which previews the ID.
#' @param id2 The id of the textOutput, which previews the description.
#'
#' @noRd
add_then_preview <- function(id1, id2) {
  tagList(
    h5("THEN ID") %>%
      prompter::add_prompt(position = "left", message = "Previews the ID combination for the THEN statement."),
    wellPanel(textOutput(id1), style = "padding: 5px; background: #ffffff; border-color: #95a5a6"),
    h5("THEN Description") %>%
      prompter::add_prompt(position = "left", message = "Previews the combined description for the THEN statement."),
    wellPanel(textOutput(id2), style = "padding: 5px; background: #ffffff; border-color: #95a5a6"),
  )
}
