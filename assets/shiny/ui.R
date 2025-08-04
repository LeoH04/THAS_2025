# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
source("global.R")


ui <- fluidPage(
  titlePanel("AIS Vessel Tracking"),
  sidebarLayout(
    sidebarPanel(
      textInput("mmsi", "Enter MMSI:", value = "412420898"),
      actionButton("go", "Load Vessel"),
      hr(),
      checkboxInput("show_pred", "Show 1-min predictions", value = FALSE),
      numericInput(
        inputId = "delta_t",
        label = "Prediction time step Δt (minutes)",
        value = 1,
        min = 1,
        max = 30,
        step = 1
      )
    ),
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)