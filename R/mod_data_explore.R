#' data_explore UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_explore_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(type = "tabs",
                tabPanel("Links Data", DT::dataTableOutput(ns("links_data_raw_out"))),
                tabPanel("Event Data", DT::dataTableOutput(ns("event_data_raw_out")))
    )

  )
}

#' data_explore Server Function
#'
#' @noRd
mod_data_explore_server <- function(input, output, session,
                                    input_data_links,
                                    input_data_events){
  ns <- session$ns
  # print(input_dfs())

  #input_dfs is list with: link_input_file ; event_input_file
  output$links_data_raw_out <- DT::renderDataTable({
    # print(input_data_links)
    # print(dplyr::glimpse(input_data_links))
    DT::datatable(input_data_links,
                  escape = FALSE,
                  filter = 'top',
                  selection = 'none')
  })

  output$event_data_raw_out <- DT::renderDataTable({
    # print(dplyr::glimpse(input_data_events))
    DT::datatable(input_data_events,
                  escape = FALSE,
                  filter = 'top',
                  selection = 'none')
  })


}

## To be copied in the UI
# mod_data_explore_ui("data_explore_ui_1")

## To be copied in the server
# callModule(mod_data_explore_server, "data_explore_ui_1")

