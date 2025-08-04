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

path <- getwd()

source(paste0(path, "/02_code/Functions/forecast_functions.R"))

base_url <- "https://aidaho-edu.uni-hohenheim.de/aisdb"

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Main Script--------------------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Fetch the data for the vessel
mmsi_target <- 412420898

cat("Fetching data...\n")
df_dyn <- fetch_ais_data(base_url, "ais_dynamic", mmsi_target)

# Clean and order the fetched data
df_dyn <- df_dyn %>%
  filter(!is.na(latitude), !is.na(longitude), latitude != 0, longitude != 0) %>%
  arrange(msg_timestamp)

#Calculate and output the MSPE for the old model and the new welford approach
cat("Calculating original 10-min forecast MSPE...\n")
mspe_orig_10 <- calculate_mspe(df_dyn, delta_minutes = 10)
cat("Original Model 10-min MSPE:", round(mspe_orig_10, 4), "NM²\n\n")

cat("Calculating Welford running average 10-min forecast MSPE...\n")
mspe_welford_10 <- forecast_positions_welford(df_dyn, delta_minutes = 10)
cat("Welford Running Avg Forecast 10-min MSPE:", round(mspe_welford_10, 4), "NM²\n")