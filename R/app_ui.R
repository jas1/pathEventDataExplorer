#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    dashboard_definition()
  )
}

dashboard_definition <- function(){

  shinydashboardPlus::dashboardPagePlus(

    header = header_definition(),      # Create our navigation menu that links to each of the tabs we defined
    sidebar = sidebar_definition(),# Show the appropriate tab's content in the main body of our dashboard when we select it
    body = body_definition(),
    rightsidebar = NULL,
    title = "Path Event Data Explorer",
    skin = "green"
  )

}

header_definition <- function(){
  shinydashboardPlus::dashboardHeaderPlus(
    title = "Path Event Data Explorer",
    enable_rightsidebar = FALSE
  )
}

sidebar_definition <- function(){
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      shinydashboard::menuItem("Upload", tabName = "data_load", icon = icon("upload")),
      shinydashboard::menuItem("Data Explore", tabName = "data_exp", icon = icon("th")),
      shinydashboard::menuItem("Data Path Viz", tabName = "data_viz", icon = icon("share-alt"))
    )
  )
}

body_definition <- function(){
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem("data_load",
                              div(
                                div(
                                  h2("Upload Link data"),
                                  textInput("link_data_sheet", "Excel Sheet", "links"),
                                  fileInput("link_data_file_upload","Select a file")
                                ),
                                div(
                                  h2("Upload Event data"),
                                  textInput("event_data_sheet", "Excel Sheet", "events"),
                                  fileInput("event_data_file_upload","Select a file")
                                )
                              )

                              # mod_data_load_ui("mod_data_load_ui_1")
                              ),
      shinydashboard::tabItem("data_exp", mod_data_explore_ui("mod_data_explore_ui_1")),
      shinydashboard::tabItem("data_viz", mod_data_viz_ui("mod_data_viz_ui_1")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'pathEventDataExplorer'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

