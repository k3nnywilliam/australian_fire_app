box::use(
  shiny[bootstrapPage,navbarPage,tabPanel, moduleServer, NS, reactiveValues],
)

box::use(
  app/view/fire_map
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    fire_map$ui(ns("firemap"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    fire_map$server("firemap")
  })
}
