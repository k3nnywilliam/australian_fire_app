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
    cat("Loading data...\n", file = stderr())
    fire_nrt_V1_96617_df <- readr::read_csv('app/data/fire_nrt_V1_96617.csv') |> data.table::as.data.table()
    #fire_archive_V1_96617_df <- readr::read_csv('app/data/fire_archive_V1_96617.csv') |> data.table::as.data.table()
    fire_nrt_M6_96619_df <- readr::read_csv('app/data/fire_nrt_M6_96619.csv') |> data.table::as.data.table()
    #fire_archive_M6_96619_df <- readr::read_csv('app/data/fire_archive_M6_96619_mod.csv') |> data.table::as.data.table()
    selected_data <- reactiveValues(val = fire_nrt_V1_96617_df)
    cat("Loading data done.\n", file = stderr())
    fire_map$server("firemap", selected_data,  fire_nrt_V1_96617_df, fire_nrt_M6_96619_df)
  })
}
