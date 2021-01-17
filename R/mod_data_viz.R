#' data_viz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_viz_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      fluidRow(
        # UI : Input Select Layout --------------------------------------------------------------
        column(4, selectizeInput(inputId = ns("input_category_select"), label = "category", choices = '',
                                 options = list(
                                   placeholder = 'Select Category',
                                   # options = list(
                                   #     # 'actions-box' = TRUE,
                                   #     # size = 10,
                                   #     # 'deselect-all-text' = "None",
                                   #     # 'select-all-text' = "All",
                                   #     # 'none-selected-text' = "Sin Selección",
                                   #     # 'count-selected-text' = "{0} seleccionados."
                                   # ),
                                   onInitialize = I('function() { this.setValue(""); }')),
                                 multiple = TRUE)
        ),
        column(4,
               # UI : slider date --------------------------------------------------------------
               # https://shiny.rstudio.com/reference/shiny/latest/updateSliderInput.html
               sliderInput(ns("input_date_range"), "Date range:",
                           min =  lubridate::today() - lubridate::years(2) ,
                           max =  lubridate::today() + lubridate::days(10),
                           value = c(lubridate::today()-lubridate::years(1), lubridate::today()),
                           timeFormat="%Y-%m-%d")) #,
        # column(4,
        #        actionButton("input_apply_filters", "Apply Filter"))
      ),# END FLUID ROW
      fluidRow(
        # UI : network  --------------------------------------------------------------
        column(6,visNetwork::visNetworkOutput(ns("network_out"))),
        # UI : network data  --------------------------------------------------------------
        column(6,
               tabsetPanel(type = "tabs",
                           tabPanel("Node Count Data", DT::dataTableOutput(ns("network_data_out"))),
                           tabPanel("Selected Node Data", DT::dataTableOutput(ns("filtered_node_data_out")))
               ))
      )
    )
  )
}

#' data_viz Server Function
#'
#' @noRd
mod_data_viz_server <- function(input, output, session,
                                input_data_links,
                                input_data_events){
  ns <- session$ns


  links_dataset_reactive <- reactive({
    # adapt this for input data
    input_data_links
  })

  # TODO: mandar al modulo  principal
  event_dataset_reactive <- reactive({
    # adapt this for input data
    result <- input_data_events

    if (nrow(result)>0) {
       filters <- current_filters_reactive()
       if (!is.null(filters)) {
         # print(filters)

         if (!is.null(filters$categories)) {
           result <- result %>%
             filter(category %in% filters$categories)
         }

         if (!is.null(filters$date_range)) {
           # print(" date range raro")
           # print(filters$date_range)
           # print( " -------------------- ")
           # print(result$date)

           # TODO: FIX HERE:
           # Warning: Error in >=.default: comparison (5) is possible only for atomic and list types

           # result <- result %>%
           #   filter(date >= filters$date_range[1]  & date <= filters$date_range[2])
         }
       }
    }else{
      golem::cat_dev("event databse without data")
    }


    result
  })


  categories_reactive <- reactive({
    categories_out <- input_data_events %>%
      dplyr::distinct(category) %>%
      dplyr::pull(category)
    # categories_out <- c("All", categories_out)

    categories_out
  })
  observe({
    updateSelectizeInput(session, "input_category_select", choices = categories_reactive())
  })


  observe({
    range_date <- event_dataset_reactive() %>% dplyr::mutate(date=lubridate::ymd(date)) %>% dplyr::pull (date) %>% range()

    min_range <- range_date[1] - lubridate::days(10)
    max_range <- range_date[2] + lubridate::days(10)

    current_min_val <- dplyr::if_else(input$input_date_range[1] < min_range, min_range,input$input_date_range[1])
    current_max_val <- dplyr::if_else(input$input_date_range[2] > max_range, max_range,input$input_date_range[2])

    current_value_range <- c(current_min_val,current_max_val)

    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "input_date_range",
                      value = current_value_range,
                      min = min_range,
                      max = max_range,
                      step = 1)
  })

  # server - network out ----------------------------------------------------

  current_filters_reactive <- reactive({
    filters <- list("categories" = input$input_category_select,
                    "date_range"= input$input_date_range)
    filters
  })

  # observeEvent(input$input_apply_filters, {
  #     current_filters_reactive()
  #     graph_procesed_reactive()
  #     # session$sendCustomMessage(type = 'testmessage',
  #     #                           message = 'Thank you for clicking')
  # })

  graph_procesed_reactive <- reactive({
    tmp_out <- make_network(links_dataset_reactive(),
                            event_dataset_reactive(),
                            filters=current_filters_reactive())
  })

  graph_data_reactive <- reactive({
    graph_procesed_reactive()$graph_out
  })

  graph_vis_reactive <- reactive({
    graph_procesed_reactive()$vis_out
  })

  output$network_out <- visNetwork::renderVisNetwork({
    graph_vis_reactive()
  })

  output$network_data_out <- DT::renderDataTable({
    # print( graph_data_reactive())


    node_info <- graph_data_reactive() %>%
      tidygraph::activate(nodes) %>%
      tibble::as_tibble() %>%
      dplyr::rename(count=size) %>%
      dplyr::select(name,count)
    # print(node_info)
    DT::datatable(node_info,
                  escape = FALSE,
                  filter = 'top',
                  selection = 'none')
  })

  output$filtered_node_data_out <- DT::renderDataTable({

    DT::datatable(filtered_nodes_reactive(),
                  escape = FALSE,
                  filter = 'top',
                  selection = 'none')
  })


  # TODO: ADD FILTER if click , select data related to that node.
  # TODO: ADD FILTER: if select specific "campaing" , filter the data send to make the graph
  # TODO: ADD FILTER: if filtered between dates: filter data between dates.

  filtered_nodes_reactive <- reactive({
    req(input$input_network_click_node)
    # input.input_static_network_click_vertex
    # input.input_static_network_click_edge
    print(input$input_network_click_node)
    input_node <- input$input_network_click_node
    print(input_node)
    # current_node <- get_node_from_click_node(grafo = graph_data_reactive(),
    #                          input_node = input_node)
    #
    # current_node
    current_db <- event_dataset_reactive()

    ret <- get_node_data(input_node,current_db)
    ret
  })


}
# SERVER END --------------------------------------------------------------




## To be copied in the UI
# mod_data_viz_ui("data_viz_ui_1")

## To be copied in the server
# callModule(mod_data_viz_server, "data_viz_ui_1")


make_network <- function(graph_links,graph_counts_data,filters=NULL){
  # armar_grafo_grafico <- armar_str_grafico  #%>%
  # mutate(shape="icon") %>%
  # mutate(icon.face = 'Ionicons') %>%
  # mutate(icon.code=case_when(type=="mail"~"mail-outline",
  #                            type=="call"~"call-outline",
  #                            TRUE~"ellipse-outline"))
  #<ion-icon name="mail-outline"></ion-icon>
  # <ion-icon name="call-outline"></ion-icon>
  # <ion-icon name="ellipse-outline"></ion-icon>
  # armar_grafo_grafico %>% select(message)


  # graph_counts_data <- current_data_sample
  # graph_links <-  current_data_2

  counted_data <- graph_counts_data %>% dplyr::count(event)
  if (!is.null(filters)) {
    counted_data <- graph_counts_data %>%
      dplyr::filter(category %in% filters$categories) %>%
      dplyr::count(event)

  }

  counted_data <- graph_counts_data %>% dplyr::count(event)
  # counted_data %>% View()
  # graph_node_data_merged <- graph_links %>%
  #     left_join(counted_data,by=c("evento"=""))

  igraph_edgelist_2 <- tidygraph::as_tbl_graph(graph_links,directed = TRUE)
  # nombres <- igraph::V(igraph_edgelist_2)$name %>% tibble::enframe(name = NULL,value = "entidades")
  # attr_node_color <- nombres %>% left_join(nodes_colors) %>% pull(color)
  # igraph::V(igraph_edgelist_2)$color <- attr_node_color

  igraph_edgelist_3 <-  igraph_edgelist_2 %>%
    tidygraph::activate(nodes) %>%
    dplyr::left_join(counted_data,by=c("name"="event")) %>%
    dplyr::rename(size=n) %>%
    dplyr::mutate(title=paste0(name,"<br/>",
                        "Count: ",size)) %>%
    dplyr::mutate(color=dplyr::if_else(is.na(size),"#9cb2ba","#b8ffe0"))
  # https://www.color-hex.com/color-palette/102951
  # https://www.color-hex.com/color-palette/5526

  random_seed <- 12345
  # c('layout_nicely',
  # 'layout_in_circle',
  # 'layout_as_tree',
  # 'layout_on_grid',
  # 'layout_with_lgl',
  # 'layout_with_mds',
  # 'layout_with_fr',
  # 'layout_with_graphopt',
  # 'layout_with_kk',
  # 'layout_with_sugiyama')
  layout_current <- "layout_nicely" #default
  # layout_current <- "layout_as_tree"

  network_ret <- igraph_edgelist_3 %>% visNetwork::visIgraph(randomSeed = random_seed ) %>%
    # visNetwork::visNodes(size = 10) %>%
    visNetwork::visIgraphLayout(randomSeed = random_seed,layout = layout_current )  %>%
    # visNetwork::addIonicons()
    visNetwork:::visEvents(click = "function(clickEvent){
                      nodesVar = clickEvent.nodes[0];
                      edgesVar = clickEvent.edges[0];
                      if (nodesVar == null & edgesVar == null){
                      //Shiny.onInputChange('input_network_click_node', '');
                      //Shiny.onInputChange('input_network_click_link', '');
                      }

                      if (nodesVar != null){
                      Shiny.onInputChange('input_network_click_node', nodesVar);
                      Shiny.onInputChange('input_network_click_edge', '');
                      }

                      if (nodesVar == null & edgesVar != null){
                      Shiny.onInputChange('input_network_click_edge', edgesVar);
                      Shiny.onInputChange('input_network_click_node', '');
                      }
                      //alert(nodesVar,edgesVar);
                      ;}")



  out <- list("graph_out"=igraph_edgelist_3,
              "vis_out"=network_ret)

  out
}


get_node_data <- function(input_node_name,current_db,filters=NULL){

  print(input_node_name)
  filtered <- current_db %>% dplyr::filter(event==input_node_name)

  if (!is.null(filters)) {
    filtered <- filtered %>%
      dplyr::filter(category %in% filters$categories)
  }


  filtered
}
