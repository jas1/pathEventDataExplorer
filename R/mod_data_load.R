#' data_load UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_load_ui <- function(id){
  ns <- NS(id)
  tagList(


    div(
      h2("Upload Link data"),
      textInput(ns("link_data_sheet"), "Excel Sheet", "links"),
      fileInput(ns("link_data_file_upload"),"Select a file")
    ),
    div(
      h2("Upload Event data"),
      textInput(ns("event_data_sheet"), "Excel Sheet", "events"),
      fileInput(ns("event_data_file_upload"),"Select a file")
    )

    # tabsetPanel(type = "tabs",
    #             tabPanel("Upload Link data", DT::dataTableOutput("network_data_out")),
    #             tabPanel("Upload Link data", DT::dataTableOutput("filtered_node_data_out"))
    )
}

#' data_load Server Function
#' link_input_file ; event_input_file
#' @noRd
mod_data_load_server <- function(input, output, session){

  ns <- session$ns

  return(
    list(
      link_input_file = reactive({
          # req(input$link_data_file_upload)
          # req(input$link_data_sheet)
          #
          # print(input$link_data_sheet)

          read_data_out <- process_input_file(input$link_data_file_upload,sheet_to_process=input$link_data_sheet)
          # golem::cat_dev("mod upload link")
          # print(read_data_out)
          read_data_out
        }),

      event_input_file = reactive({
        # req(input$event_data_file_upload)
        # req(input$event_data_sheet)
        # golem::cat_dev("mod upload event")
        # print(input$event_data_sheet)

        read_data_out <- process_input_file(input$event_data_file_upload,sheet_to_process=input$event_data_sheet)
        if (nrow(read_data_out)==0) {
          read_data_out <-data.frame(id=c("123"),category=c("123"),event=c("123"),	date=c(lubridate::now()),stringsAsFactors = FALSE)
        }

        read_data_out
      })
    )
  )

}

## To be copied in the UI
# mod_data_load_ui("data_load_ui_1")

## To be copied in the server
# callModule(mod_data_load_server, "data_load_ui_1")


process_input_file <- function(file_to_process,sheet_to_process="links"){

  #print(file_to_process)

  if (is.null(file_to_process)) {
    return("")
  }

  ret <- readxl::read_xlsx(file_to_process$datapath,sheet = sheet_to_process)
  ret
}
