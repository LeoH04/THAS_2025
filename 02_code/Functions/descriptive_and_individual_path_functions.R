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
#---Function to sample data--------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sample_ais_data <- function(base_url, date = "2024-01-24", max_obs = 1000) {
  total <- 0
  result <- data.frame()
  
  while (total < max_obs) {
    # Sample a random minute between 00:00 and 23:55
    minute <- sample(0:(24*60 - 5), 1)
    # Start time based on the date
    start <- as.POSIXct(date, tz = "UTC") + minutes(minute)
    # End time for the 5 minute interval
    end <- start + minutes(5)
    
    # Format times in ISO 8601 format for the API query
    start_iso <- format(start, "%Y-%m-%dT%H:%M:%SZ")
    end_iso <- format(end, "%Y-%m-%dT%H:%M:%SZ")
    
    # Query the API to output max. 100 rows in the specified interval
    url <- paste0(
      base_url, "/ais_dynamic?",
      "msg_timestamp=gte.", start_iso,
      "&msg_timestamp=lt.", end_iso,
      "&limit=100"
    )
    
    response <- GET(url)
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    # Update the results to the growing data frame
    result <- bind_rows(result, data)
    total <- nrow(result)
    # Avoid spamming the API 
    Sys.sleep(0.2)
  }
  return(result)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Function to create descriptive statistics--------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

create_all_descriptives <- function(df) {
  # Calculate basic descriptive statistics for every numeric column, NaN columns are ignored
  numeric_desc <- df %>%
    summarise(across(where(is.numeric), list(
      count = ~sum(!is.na(.)),
      mean  = ~mean(., na.rm = TRUE),
      sd    = ~sd(., na.rm = TRUE),
      min   = ~min(., na.rm = TRUE),
      max   = ~max(., na.rm = TRUE)
    ), .names = "{.col}_{.fn}")) %>%
    # Restructure the data to make it readable
    pivot_longer(everything(),
                 names_to = c("variable", "stat"),
                 names_sep = "_",
                 values_to = "value") %>%
    pivot_wider(names_from = stat, values_from = value)
  
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
# Select non-numeric columns
non_numeric_cols <- df %>% select(where(~!is.numeric(.)))
# Calculate basic descriptive statistics for every non-numeric column
  non_numeric_desc <- map_dfr(names(non_numeric_cols), function(col_name) {
    col_data <- non_numeric_cols[[col_name]]
    tibble(
      variable = col_name,
      count = sum(!is.na(col_data)),
      unique = n_distinct(col_data, na.rm = TRUE),
      top = names(sort(table(col_data), decreasing = TRUE))[1],
      top_count = max(table(col_data))
    )
  })
  
  # Return both as a named list
  return(list(
    numeric = numeric_desc,
    non_numeric = non_numeric_desc
  ))
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Function to visualize AIS points-----------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plot_ais_points <- function(df) {
  # Filter for valid coordinates (required to avoid leaflet warnings)
  df <- df %>%
    filter(
      !is.na(latitude), !is.na(longitude),
      latitude >= -90, latitude <= 90,
      longitude >= -180, longitude <= 180
    )
  
  # Create color palette for speeds ≤ 100
  pal <- colorNumeric("viridis", domain = df$speed[df$speed <= 100], na.color = NA)
  
  # Split data
  df_valid <- df[!is.na(df$speed) & df$speed <= 100, ]
  df_high  <- df[!is.na(df$speed) & df$speed > 100, ]
  df_na    <- df[is.na(df$speed), ]
  
  # Initialize map
  map <- leaflet() %>%
    addTiles()
  
  # Valid speeds (≤100 knots) with viridis palette
  if (nrow(df_valid) > 0) {
    map <- map %>%
      addCircleMarkers(
        data = df_valid,
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~pal(speed),
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = ~paste0(
          "MMSI: ", mmsi, "<br>",
          "Speed: ", speed, " knots<br>",
          "Latitude: ", latitude, "<br>",
          "Longitude: ", longitude, "<br>",
          "Timestamp: ", msg_timestamp
        )
      )
  }
  
  # High-speed vessels (>100 knots) in red
  if (nrow(df_high) > 0) {
    map <- map %>%
      addCircleMarkers(
        data = df_high,
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = "red",
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = ~paste0(
          "MMSI: ", mmsi, "<br>",
          "Speed: ", speed, " knots<br>",
          "Latitude: ", latitude, "<br>",
          "Longitude: ", longitude, "<br>",
          "Timestamp: ", msg_timestamp,
          "<br><b>Note:</b> Speed > 100 knots"
        )
      )
  }
  
  # Missing speeds in gray
  if (nrow(df_na) > 0) {
    map <- map %>%
      addCircleMarkers(
        data = df_na,
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = "gray",
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = ~paste0(
          "MMSI: ", mmsi, "<br>",
          "Speed: NA<br>",
          "Latitude: ", latitude, "<br>",
          "Longitude: ", longitude, "<br>",
          "Timestamp: ", msg_timestamp,
          "<br><b>Note:</b> Missing speed"
        )
      )
  }
  
  # Legend for speed ≤ 100
  map <- map %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = df_valid$speed,
      title = "Speed (≤ 100 knots)"
    )
  
  # Custom legend for >100 and NA
  map <- map %>%
    addControl(html = "
      <div style='background:white; padding:10px; border-radius:5px'>
        <b>Legend:</b><br>
        <div><span style='display:inline-block; width:12px; height:12px; background:red; margin-right:5px;'></span>Speed > 100 knots</div>
        <div><span style='display:inline-block; width:12px; height:12px; background:gray; margin-right:5px;'></span>Speed missing</div>
      </div>
    ", position = "bottomleft")
  
  return(map)
}



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Function to visualize vessel paths--------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plot_vessel_path <- function(data, color = "blue", cluster = FALSE, mmsi = NULL) {
  # Create a legend label that includes the MMSI value
  mmsi_label <- paste("MMSI:", mmsi)
  
  # Initialize a Leaflet map using the vessel data
  map <- leaflet(data) %>%
    addTiles() %>%  # Add default OpenStreetMap tiles as the base layer
    
    # Draw polylines to connect the vessel's GPS points (shows trajectory/path)
    addPolylines(
      lng = ~longitude,  # Longitude for horizontal axis
      lat = ~latitude,   # Latitude for vertical axis
      color = color,     # Line color (default is blue)
      weight = 2,        # Line thickness
      opacity = 0.7      # Line transparency
    ) %>%
    
    # Add circular markers at each GPS point along the path
    addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      popup = ~paste0(
        "Speed: ", speed, " knots<br>",
        "Latitude: ", latitude, "<br>",
        "Longitude: ", longitude, "<br>",
        "Timestamp: ", msg_timestamp),  # Info shown on click
      radius = 4,           # Marker size
      color = color,        # Marker color
      fillOpacity = 0.5,    # Transparency of marker fill
      clusterOptions = if (cluster) markerClusterOptions() else NULL
      # If cluster = TRUE, enable clustering of nearby markers
    ) %>%
    
    # Add a legend at the bottom right showing what the color represents
    addLegend("bottomright",
              colors = color,         # Color in the legend
              labels = mmsi_label,    # Text label (includes MMSI)
              title = "Vessel Path")  # Title of the legend
  
  return(map)  # Return the finished Leaflet map
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