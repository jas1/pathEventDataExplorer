#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here

  ## link_input_file ; event_input_file
  # data_load_input_files_reactive <- reactive({
  #   callModule(mod_data_load_server, "data_load_ui_1")
  # })

  # ideally will come from  data load module
  # couldnt make it hhappen
  link_input_file_reactive  <-  reactive({
    req(input$link_data_file_upload)
    req(input$link_data_sheet)

    # print(input$link_data_sheet)

    read_data_out <- process_input_file(input$link_data_file_upload,sheet_to_process=input$link_data_sheet)
    # golem::cat_dev("mod upload link")
    # print(read_data_out)
    read_data_out
  })

  # ideally will come from  data load module
  # couldnt make it hhappen
  event_input_file_reactive <-  reactive({
    req(input$event_data_file_upload)
    # req(input$event_data_sheet)
    golem::cat_dev("mod upload event")
    # print(input$event_data_sheet)

    read_data_out <- process_input_file(input$event_data_file_upload,sheet_to_process=input$event_data_sheet)
    if (nrow(read_data_out)==0) {
      read_data_out <-data.frame(id=c("123"),category=c("123"),event=c("123"),	date=c(lubridate::now()),stringsAsFactors = FALSE)
    }

    read_data_out
  })


  # reactive_printing <- reactive({
  #   golem::cat_dev("reactive_printing")
  #   # print(data_load_input_files_reactive()$link_input_file)
  #   # print(data_load_input_files_reactive()$event_input_file)
  # })


  # TODO: mandar como input del componente de ver datos
  # data_load_input_files para mostrarlos
  callModule(mod_data_explore_server, "mod_data_explore_ui_1",
             input_data_links=link_input_file_reactive(),
             input_data_events=event_input_file_reactive())


  # TODO: los dataset reactive mandarlos aca:no cro que haga falta porque hay que mandarle los datos.
  # luego eso se los paso al componente de visualizacion de red.
  callModule(mod_data_viz_server, "mod_data_viz_ui_1",
             input_data_links=link_input_file_reactive(),
             input_data_events=event_input_file_reactive())

}


process_input_file <- function(file_to_process,sheet_to_process="links"){

  #print(file_to_process)

  if (is.null(file_to_process)) {
    return("")
  }

  ret <- readxl::read_xlsx(file_to_process$datapath,sheet = sheet_to_process)
  ret
}
