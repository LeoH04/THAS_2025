#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Libraries and Cleaning---------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Clear workspace and graphs
if (!is.null(dev.list())) dev.off()
rm(list = ls())

# Load necessary libraries
library(httr)
library(jsonlite)
library(tidyverse)
library(leaflet)
library(mapview)
library(knitr)
library(kableExtra)

path <- getwd()

source(paste0(path, "/02_code/Functions/descriptive_and_individual_path_functions.R"))

base_url <- "https://aidaho-edu.uni-hohenheim.de/aisdb"
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Main--------------------------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Specify which Vessels we want to look at
mmsis <- c(2579999, 412420898)
data_list <- list()
static_df <- data.frame()

# Fetch both tables for each MMSI
for (mmsi in mmsis) {
  dyn <- fetch_ais_data(base_url, "ais_dynamic", mmsi)
  stat <- fetch_ais_data(base_url, "ais_static", mmsi)
  
  dyn$mmsi <- mmsi
  dyn$source <- "dynamic"
    
  # Filter invalid coordinates
  dyn <- dyn %>% filter(!is.na(latitude), !is.na(longitude), latitude != 0, longitude != 0)
  
  # Append the data to the list  
  data_list[[as.character(mmsi)]] <- dyn
  
  # Append static data
  static_df <- rbind(static_df, stat)
  
}

# Convert all columns to character for safe pivoting
static_df_char <- static_df %>%
  mutate(across(everything(), as.character))

# Pivot to long format with variables as rows and MMSIs as columns
static_df_long_wide <- static_df_char %>%
  pivot_longer(
    cols = -mmsi,               # keep 'mmsi' values as identifiers
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = mmsi,
    values_from = Value
  )

# Create LaTeX table with MMSIs as columns
latex_table_static <- kable(
  static_df_long_wide,
  format = "latex",
  booktabs = TRUE,
  caption = "Static Vessel Information from \\texttt{ais\\_static} (Long Format)",
  col.names = c("Variable", as.character(mmsis))
) %>%
  kable_styling(latex_options = "HOLD_position", font_size = 9)

print(latex_table_static)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Visualize with Leaflet---------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Define the colors for each vessel
mmsi_colors <- c("2579999" = "red", "412420898" = "blue")

for (mmsi_str in names(data_list)) {
  data <- data_list[[mmsi_str]] %>% arrange(msg_timestamp)
  color <- mmsi_colors[[mmsi_str]]
  cluster <- mmsi_str == "2579999"
  
  # Pass the parameters for each vessel to the function and plot the map
  m_path <- plot_vessel_path(data, color = color, cluster = cluster, mmsi = mmsi_str)
  mapshot(m_path, file = paste0(path, paste0("/03_report/graphs/map_path_", mmsi_str, ".pdf")))
  print(m_path)
  
}