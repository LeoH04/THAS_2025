#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Libraries and Cleaning---------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Clear workspace and graphs
if (!is.null(dev.list())) dev.off()
rm(list = ls())

# Load necessary libraries
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(tidyr)
library(leaflet)
library(mapview)

path <- getwd()

source(paste0(path, "/02_code/Functions/forecast_functions.R"))

base_url <- "https://aidaho-edu.uni-hohenheim.de/aisdb"


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Main Script--------------------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Fetch the data for the vessel
mmsi_target <- 412420898

df_dyn <- fetch_ais_data(base_url, "ais_dynamic", mmsi_target)

df_dyn <- df_dyn %>%
  filter(!is.na(latitude), !is.na(longitude), latitude != 0, longitude != 0) %>%
  arrange(msg_timestamp)

# Compute MSPEs
mspe_1  <- calculate_mspe(df_dyn, delta_minutes = 1)
mspe_5  <- calculate_mspe(df_dyn, delta_minutes = 5)
mspe_10 <- calculate_mspe(df_dyn, delta_minutes = 10)

naive_1  <- calculate_naive_mspe(df_dyn, delta_minutes = 1)
naive_5  <- calculate_naive_mspe(df_dyn, delta_minutes = 5)
naive_10 <- calculate_naive_mspe(df_dyn, delta_minutes = 10)

# Print MSPE results
cat("MSPE Forecast Δt = 1 min:", round(mspe_1, 4), "NM²\n")
cat("MSPE Forecast Δt = 5 min:", round(mspe_5, 4), "NM²\n")
cat("MSPE Forecast Δt = 10 min:", round(mspe_10, 4), "NM²\n\n")

cat("Naïve MSPE Δt = 1 min:", round(naive_1, 4), "NM²\n")
cat("Naïve MSPE Δt = 5 min:", round(naive_5, 4), "NM²\n")
cat("Naïve MSPE Δt = 10 min:", round(naive_10, 4), "NM²\n")

# Plot for Δt = 10min forecast
df_forecasted <- forecast_positions(df_dyn, time_step_minutes = 1)
map_pred_act <- plot_forecast_vs_actual(df_forecasted)
mapshot(map_pred_act, file = paste0(path, "/03_report/graphs/map_pred_act.pdf"))
print(map_pred_act)
