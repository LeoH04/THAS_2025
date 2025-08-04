#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Libraries and Cleaning---------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Clear workspace and graphs
if(!is.null(dev.list())) dev.off()
rm(list = ls())

# Load necessary libraries
library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(leaflet)

path <- getwd()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Function to Forecast Positions-------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Forecast the positions using the provided formulas of the task sheet
forecast_positions <- function(df, time_step_minutes = 1) {
  dt <- time_step_minutes / 60  # Convert to hours (knots = NM/hr)
  
  df <- df %>%
    arrange(msg_timestamp) %>%
    mutate(
      #Convert vessel course (degrees from north) into radians
      theta_rad = course * pi / 180,
      delta_x = speed * dt * sin(theta_rad),
      delta_y = speed * dt * cos(theta_rad),
      lat_pred = latitude + delta_y / 60,
      lon_pred = longitude + delta_x / (60 * cos(latitude * pi / 180))
    )
  
  return(df)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Function: Great-Circle Distance (nautical miles)-------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# We implement the formula using the one provided on the task sheet
great_circle_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 3440  # nautical miles
  to_rad <- pi / 180
  
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  lat1 <- lat1 * to_rad
  lat2 <- lat2 * to_rad
  
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * asin(sqrt(a))
  
  R * c
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Function: MSPE Calculation-----------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# We calculate the MSPE using the great-circle-distance
calculate_mspe <- function(df, delta_minutes) {
  df_forecast <- forecast_positions(df, time_step_minutes = delta_minutes)
  
  df_forecast <- df_forecast %>%
    mutate(
      lat_next = lead(latitude, n = delta_minutes),
      lon_next = lead(longitude, n = delta_minutes)
    ) %>%
    filter(!is.na(lat_next), !is.na(lon_next)) %>%
    mutate(
      dist_error = great_circle_distance(lat_pred, lon_pred, lat_next, lon_next),
      squared_error = dist_error^2
    )
  
  mean(df_forecast$squared_error)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Function: Naïve MSPE (last observation)---------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# We calculate the MSPE taking the last availabe position as prediction
calculate_naive_mspe <- function(df, delta_minutes) {
  df <- df %>%
    arrange(msg_timestamp) %>%
    mutate(
      lat_pred = latitude,
      lon_pred = longitude,
      lat_next = lead(latitude, n = delta_minutes),
      lon_next = lead(longitude, n = delta_minutes)
    ) %>%
    filter(!is.na(lat_next), !is.na(lon_next)) %>%
    mutate(
      dist_error = great_circle_distance(lat_pred, lon_pred, lat_next, lon_next),
      squared_error = dist_error^2
    )
  
  mean(df$squared_error)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Function: Leaflet Map of Actual vs Predicted----------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
plot_forecast_vs_actual <- function(df) {
  leaflet() %>%
    addTiles() %>%
    
    # Actual path (blue line)
    addPolylines(data = df,
                 lng = ~longitude, lat = ~latitude,
                 color = "blue", weight = 2, opacity = 0.7, group = "Actual Path") %>%
    
    # Forecasted path (red line)
    addPolylines(data = df,
                 lng = ~lon_pred, lat = ~lat_pred,
                 color = "red", weight = 2, opacity = 0.7, group = "Forecasted Path") %>%
    
    # Actual position points (blue)
    addCircleMarkers(data = df,
    lng = ~longitude, lat = ~latitude,
    color = "blue", radius = 4, stroke = FALSE,
    fillOpacity = 0.7, group = "Actual Points",
    label = ~paste("Actual:", msg_timestamp)) %>%
    
    # Forecasted position points (red)
    #addCircleMarkers(data = df,
    #lng = ~lon_pred, lat = ~lat_pred,
    #color = "red", radius = 4, stroke = FALSE,
    #fillOpacity = 0.7, group = "Forecast Points",
    #label = ~paste("Forecast:", msg_timestamp)) %>%
    
    # Legend for clarity
    addLegend("bottomright",
              colors = c("blue", "red"),
              labels = c("Actual", "Forecast"),
              title = "Track")
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Function to Fetch AIS Data-----------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Query the API to output all rows for the specified vessel
fetch_ais_data <- function(base_url, table, mmsi) {
  query <- paste0(base_url, "/", table, "?mmsi=eq.", mmsi)
  response <- GET(query)
  stop_for_status(response)
  content <- content(response, "text", encoding = "UTF-8")
  return(fromJSON(content))
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Welford Running Mean for Scalar Values-----------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# We implement the welford algorithm for a running mean based on our lecture slides
welford_running_mean <- function(x) {
  n <- 0
  mean <- 0
  means <- numeric(length(x))
  
  for (i in seq_along(x)) {
    n <- n + 1
    mean <- mean + (x[i] - mean) / n
    means[i] <- mean
  }
  means
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Welford Running Mean for Angles (degrees)-------------------------------------
#---(using vector components to handle circularity)-------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Compute a numerically‐stable running average of compass headings (°) using Welford’s circular‐mean algorithm
welford_running_mean_angle <- function(angle_deg) {
  n <- 0
  mean_x <- 0
  mean_y <- 0
  mean_angles <- numeric(length(angle_deg))
  
  for (i in seq_along(angle_deg)) {
    n <- n + 1
    rad <- angle_deg[i] * pi / 180
    x <- cos(rad)
    y <- sin(rad)
    mean_x <- mean_x + (x - mean_x) / n
    mean_y <- mean_y + (y - mean_y) / n
    
    mean_angle_rad <- atan2(mean_y, mean_x)
    mean_angle_deg <- (mean_angle_rad * 180 / pi) %% 360
    mean_angles[i] <- mean_angle_deg
  }
  mean_angles
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Forecast Positions using Welford Running Average Speed and Course--------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# We predict the positions using welford and calculate the MSPE
forecast_positions_welford <- function(df, delta_minutes = 10) {
  df <- df %>% arrange(msg_timestamp)
  
  # Ensure timestamp is POSIXct
  if (!inherits(df$msg_timestamp, "POSIXct")) {
    df$msg_timestamp <- ymd_hms(df$msg_timestamp, tz = "UTC")
  }
  
  # Compute forecasted position based on smoothed speed and direction
  df <- df %>%
    mutate(
      speed_avg = welford_running_mean(speed),
      course_avg = welford_running_mean_angle(course)
    )
  
  dt <- delta_minutes / 60  # Convert minutes to hours for knots
  
  df <- df %>%
    mutate(
      #Convert vessel course (degrees from north) into radians
      theta_rad = course_avg * pi / 180,
      delta_x = speed_avg * dt * sin(theta_rad),  # NM east-west
      delta_y = speed_avg * dt * cos(theta_rad),  # NM north-south
      # Latitude and longitude predictions
      lat_pred = latitude + delta_y / 60,
      lon_pred = longitude + delta_x / (60 * cos(latitude * pi / 180)),
      # Actual future position (delta_minutes ahead)
      lat_next = lead(latitude, n = delta_minutes),
      lon_next = lead(longitude, n = delta_minutes)
    ) %>%
    filter(!is.na(lat_next), !is.na(lon_next))  # Remove rows where ground truth is missing
  
  # Calculate prediction error using great-circle distance
  df <- df %>%
    mutate(
      dist_error = great_circle_distance(lat_pred, lon_pred, lat_next, lon_next),
      squared_error = dist_error^2
    )
  
  # Return Mean Squared Prediction Error (MSPE)
  mspe <- mean(df$squared_error)
  return(mspe)
}