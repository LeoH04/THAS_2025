#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Libraries and Cleaning---------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Clear workspace and graphs
if (!is.null(dev.list())) dev.off()
rm(list = ls())

# Clear workspace and graphs
library(leaflet)
library(htmlwidgets)
library(dplyr)

path <- getwd()

source(paste0(path, "/02_code/Functions/descriptive_and_individual_path_functions.R"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Load Sample Data---------------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Read sample points CSV into a data.frame
df_samples <- read.csv(paste0(path, "/01_data/ais_dynamic_sample.csv"))

# Standardize column names to lowercase
names(df_samples) <- tolower(names(df_samples))

#Build the leaflet map
map <- plot_ais_points(df_samples)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Save Map as HTML File----------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Define where the HTML should be stored
output_file <- paste0(path, "/assets/site-content/sample_points.html")

# Store the map as HTML file
htmlwidgets::saveWidget(map, output_file, selfcontained = TRUE)