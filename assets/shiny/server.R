# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
source("global.R")


server <- function(input, output, session) {
  ais_data <- eventReactive(input$go, {
    req(input$mmsi)
    df <- get_ais_by_mmsi(input$mmsi)
    df
  })
  
  predictions <- reactive({
    df <- ais_data()
    if (!input$show_pred || nrow(df) < 2) return(NULL)
    
    dt <- input$delta_t / 60  # convert minutes to hours
    
    df <- df %>%
      arrange(msg_timestamp) %>%
      mutate(
        theta_rad = course * pi / 180,
        delta_x = speed * dt * sin(theta_rad),
        delta_y = speed * dt * cos(theta_rad),
        lat_pred = latitude + delta_y / 60,
        lon_pred = longitude + delta_x / (60 * cos(latitude * pi / 180)),
        lat_true = lead(latitude, n = input$delta_t),
        lon_true = lead(longitude, n = input$delta_t)
      ) %>%
      filter(!is.na(lat_true), !is.na(lon_true)) %>%
      select(msg_timestamp, lat_pred, lon_pred, lat_true, lon_true)
    
    return(df)
  })
  
  output$map <- renderLeaflet({
    df <- ais_data() %>% arrange(msg_timestamp)
    req(df)
    m <- leaflet(df) %>% addTiles()
    m <- m %>% addPolylines(~longitude, ~latitude, color = "blue", weight = 2)
    m <- m %>% addCircleMarkers(~longitude, ~latitude,
                                radius = ~pmin(pmax(speed / 2, 2), 8),
                                color = "navy", stroke = FALSE, fillOpacity = 0.6,
                                popup = ~paste0(
                                  "Speed: ", speed, " knots<br>",
                                  "Latitude: ", latitude, "<br>",
                                  "Longitude: ", longitude, "<br>",
                                  "Timestamp: ", msg_timestamp
                                ))%>%
      addLegend("bottomright",
                colors = "navy",
                labels = paste("MMSI:", input$mmsi),
                title = "Vessel Path")
    
    
    pred <- predictions()
    if (!is.null(pred)) {
      # Predicted positions (red)
      m <- m %>%
        addPolylines(data =pred,
                     lng = ~lon_pred, lat = ~lat_pred,
                     color = "red", weight = 2, opacity = 0.7) %>%
        #addCircleMarkers(data = pred,
                         #lng = ~lon_pred,
                         #lat = ~lat_pred,
                         #radius = 3,
                         #color = "red",
                         #fillOpacity = 0.9,
                         #popup = ~paste0("Predicted<br>Time: ", msg_timestamp)) %>%
        addLegend("bottomright",
                  colors = c("navy", "red"),
                  labels = c("Actual", "Forecast"),
                  title = "Track")
      
    }
    
    return(m)
  })
}