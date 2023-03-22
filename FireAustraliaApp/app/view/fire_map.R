box::use(
  dplyr, data.table, leaflet, reactable, utils[head, str],
  leaflet.extras[addHeatmap, addFullscreenControl],
  knitr,
  shinyWidgets, shinycssloaders, shinybusy[add_busy_bar],
  shiny[h2, h3, moduleServer, NS, tagList, fillPage, tags,
        sidebarLayout, sidebarPanel, mainPanel, dateRangeInput, fluidPage,
        fluidRow, column,sliderInput, absolutePanel, observe, observeEvent,
        reactive, reactiveValues, navbarPage, tabPanel, p, div, selectizeInput,
        bindEvent, animationOptions]

)

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      add_busy_bar(color = "#8a4af3"),
      class = 'h-container',
      h2( "Australia Fire Map"),
      p("NASA Satellite Data MODISC6 and VIIRS 375m from 2019-08-01 to 2020-01-11"),
      selectizeInput(ns("select-data-input"), label = "Input data", choices = c("VIIRS_96617", "MODIS_C6_96619"), selected ="VIIRS_96617")
    ),
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leaflet::leafletOutput(ns("mymap"), width = "100%", height = "100%"),
    absolutePanel(bottom = 60, right = 800,
                  sliderInput(ns("slide-date-range"), "Year", min = as.Date("2019-10-01"),
                              max =  as.Date("2020-01-11"),
                              value = c(as.Date("2019-10-01"), as.Date("2019-10-31")),
                              animate = animationOptions(interval = 1000, loop = FALSE)
                              ))
  )
}

#' @export
server <- function(id, selected_data, fire_nrt_V1_96617_df, fire_nrt_M6_96619_df) {
  moduleServer(id, function(input, output, session) {
    filteredDateRange <- reactive({
      selected_data$val |>
        dplyr::filter(dplyr::between(acq_date, input$"slide-date-range"[1], input$"slide-date-range"[2]))
    })

    observe({
      print(paste0("Selected input data: ", input$"select-data-input"))
      if(input$"select-data-input" == 'VIIRS_96617') {
        selected_data$val <- fire_nrt_V1_96617_df
      }
      if(input$"select-data-input" == 'MODIS_C6_96619') {
        selected_data$val <- fire_nrt_M6_96619_df
      }
    }) |> bindEvent(input$"select-data-input")

    cat("Map inititatied...\n", file = stderr())
    output$mymap <- leaflet::renderLeaflet({
      leaflet::leaflet(fire_nrt_V1_96617_df) |>
        leaflet::setView(lng = 133.281323, lat = -26.4390917,zoom = 3.5) |>
        leaflet::addProviderTiles(leaflet::providers$OpenTopoMap,
                       options = leaflet::providerTileOptions(minZoom = 4, maxZoom = 12)) |>
        addHeatmap(~longitude, ~latitude, blur = 1, max = 0.5, radius = 3) |>
        leaflet::addMiniMap(width = 100, height = 100, position = 'topright') |>
        addFullscreenControl()
    })

    observe({
      leaflet::leafletProxy("mymap", data = filteredDateRange()) |>
        leaflet.extras::clearHeatmap() |>
        addHeatmap(~longitude, ~latitude, blur = 1, max = 0.5, radius = 3)
    })

    # output$table <- reactable$renderReactable(
    #   reactable$reactable(selected_data$val[1:100,], minRows = 100, searchable = TRUE)
    # )

  })
}
