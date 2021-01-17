#test_module_upload

# Given the following module
computation_module_server <- function(input, output, session){
  ns <- session$ns
  r <- reactiveValues(
    value = NULL
  )
  observeEvent( input$selector , {
    r$value <- input$selector * 10
  })

}

# We can test it that way
library(shiny)
library(testthat)
testModule(computation_module_server, {

  # Give input$selector a value
  session$setInputs(selector = 1)
  # Call {testthat} functions
  expect_equal(r$value, 10)

  # Give input$selector a value
  session$setInputs(selector = 2)
  # Call {testthat} functions
  expect_equal(r$value, 20)


})
