#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Libraries and Cleaning---------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Clear workspace and graphs
if(!is.null(dev.list())) dev.off()
rm(list = ls())

# Load necessary libraries
library(leaflet)
library(dplyr)
library(knitr)
library(kableExtra)
library(mapview)

path <- getwd()

source(paste0(path, "/02_code/Functions/descriptive_and_individual_path_functions.R"))

base_url <- "https://aidaho-edu.uni-hohenheim.de/aisdb"

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Main code----------------------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Sample the data and set a seed for reproducability 
set.seed(123)
sampled <- sample_ais_data(base_url)

# Save the sample
write.csv(sampled, file = file.path(paste0(path, "/01_data"), "ais_dynamic_sample.csv"), row.names = FALSE)

# Generate descriptive statistics for our sample
descriptives <- create_all_descriptives(sampled)
numeric_descriptives <- descriptives$numeric
non_numeric_descriptives <- descriptives$non_numeric

# Round numeric columns and format as regular numbers (avoid e+ notation)
numeric_descriptives_clean <- numeric_descriptives %>%
  mutate(across(where(is.numeric), ~ format(round(., 4), scientific = FALSE)))

# Do the same for top_count in non-numeric
non_numeric_descriptives_clean <- non_numeric_descriptives %>%
  mutate(across(where(is.numeric), ~ format(., scientific = FALSE)))

# Create a LaTeX table for our report
numeric_table_latex <- kable(numeric_descriptives_clean, format = "latex", booktabs = TRUE,
                             caption = "Numeric Descriptive Statistics") %>%
  kable_styling(latex_options = "HOLD_position", font_size = 9)

non_numeric_table_latex <- kable(non_numeric_descriptives_clean, format = "latex", booktabs = TRUE,
                                 caption = "Non-Numeric Descriptive Statistics") %>%
  kable_styling(latex_options = "HOLD_position", font_size = 9)

print(numeric_table_latex)
print(non_numeric_table_latex)

# Plot all data points
m_all <- plot_ais_points(sampled)
mapshot(m_all, file = paste0(path, "/03_report/graphs/map_all.pdf"))
print(m_all)

# Filter satellite data and plot the data points
satellite_only <- sampled %>% filter(collection_type == "satellite")
m_satellite <- plot_ais_points(satellite_only)
mapshot(m_satellite, file = paste0(path, "/03_report/graphs/map_satellite.pdf"))
print(m_satellite)