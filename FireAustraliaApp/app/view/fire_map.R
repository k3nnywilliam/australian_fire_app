box::use(
  dplyr, data.table, leaflet, reactable,
  leaflet.extras[addHeatmap, addFullscreenControl],
  shiny[h3, moduleServer, NS, tagList, fillPage, tags,
        sidebarLayout, sidebarPanel, mainPanel, dateRangeInput, fluidPage,
        fluidRow, column,sliderInput, absolutePanel, observe, observeEvent,
        reactive, reactiveValues, navbarPage, tabPanel]

)

cat("Loading data...\n", file = stderr())
fire_nrt_V1_96617_df <- readr::read_csv('app/data/fire_nrt_V1_96617.csv') |> data.table::as.data.table()
fire_archive_V1_96617_df <- readr::read_csv('app/data/fire_archive_V1_96617.csv') |> data.table::as.data.table()
fire_nrt_M6_96619_df <- readr::read_csv('app/data/fire_nrt_M6_96619.csv') |> data.table::as.data.table()
fire_archive_M6_96619_df <- readr::read_csv('app/data/fire_archive_M6_96619_mod.csv') |> data.table::as.data.table()
cat("Loading data done.\n", file = stderr())


#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Australia Fire Map"),
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leaflet::leafletOutput(ns("mymap"), width = "100%", height = "100%"),
    absolutePanel(top = 60, right = 30,
                  sliderInput(ns("slide-date-range"), "Year", min = min(fire_nrt_V1_96617_df$acq_date),
                              max =  max(fire_nrt_V1_96617_df$acq_date),
                              value = c(as.Date("2019-10-01"), as.Date("2019-10-31"))))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    filteredDateRange <- reactive({
      fire_nrt_V1_96617_df |>
        dplyr::filter(dplyr::between(acq_date, input$"slide-date-range"[1],
                                     input$"slide-date-range"[2]))
    })

    cat("Map inititatied...\n", file = stderr())
    output$mymap <- leaflet::renderLeaflet({
      leaflet::leaflet(fire_nrt_V1_96617_df) |>
        leaflet::setView(lng = 133.281323, lat = -26.4390917,zoom = 3.5) |>
        leaflet::addProviderTiles(leaflet::providers$OpenTopoMap,
                       options = leaflet::providerTileOptions(minZoom = 4, maxZoom = 12)) |>
        addHeatmap(~longitude, ~latitude, blur = 1, max = 0.5, radius = 3) |>
        #leaflet::addMiniMap(width = 100, height = 100, position = 'topright') |>
        addFullscreenControl()
    })

    observe({
      leaflet::leafletProxy("mymap", data = filteredDateRange()) |>
        leaflet.extras::clearHeatmap() |>
        addHeatmap(~longitude, ~latitude, blur = 1, max = 0.5, radius = 3)
    })

    output$table <- reactable$renderReactable(
      reactable$reactable(fire_nrt_V1_96617_df[1:100,], minRows = 100, searchable = TRUE)
    )

  })
}
