#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Libraries and Cleaning---------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Clear workspace and graphs
if(!is.null(dev.list())) dev.off()
rm(list = ls())

# Load necessary libraries
library(httr)
library(jsonlite)
library(tidyverse)
library(knitr)
library(kableExtra)

base_url <- "https://aidaho-edu.uni-hohenheim.de/aisdb/"

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Task 2.2-----------------------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Query the API to output 10 rows of ais_static
query <- paste0(base_url, "/ais_static?limit=10")
response<- GET(query)
example_data_static <- fromJSON(content(response, "text", encoding = "UTF-8"))
print(example_data_static)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Query the API to output 10 rows of ais_dynamic
query <- paste0(base_url, "/ais_dynamic?limit=10")
response <- GET(query)
example_data_dynamic <- fromJSON(content(response, "text", encoding = "UTF-8"))
print(example_data_dynamic)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Task 2.3b----------------------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Query the API to output the number of rows in ais_static
query <- paste0(base_url, "/ais_static?select=count()")
response <- GET(query)
number_of_rows_static <- fromJSON(content(response, "text", encoding = "UTF-8"))
print(number_of_rows_static)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Query the API to output the number of rows in ais_dynamic
query <- paste0(base_url, "/ais_dynamic?select=count()")
response <- GET(query)
number_of_rows_dynamic <- fromJSON(content(response, "text", encoding = "UTF-8"))
print(number_of_rows_dynamic)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Task 2.3c----------------------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Query the API to output the the rows grouped by their flag ordered descending
query <- paste0(base_url, "/ais_static?select=count(),flag&order=count.desc")
response <- GET(query)
grouped_by_flag_static <- fromJSON(content(response, "text", encoding = "UTF-8"))
# The length of the grouped rows equals the number of flags
print(length(grouped_by_flag_static$flag))
# Output the first flag as this is the most occuring flag
print(grouped_by_flag_static$flag[[1]])

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---Task 2.3d----------------------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Query the API to output the avg, min, max and count of every numeric attribute 
columns_numeric <- c("mmsi", "imo", "draught", "ship_type_code", "length", "width")
results_list <- list()

# Loop through every attribute
for (col in columns_numeric) {
  query <- paste0(
    base_url,
    "/ais_static?select=",
    "average_value:", col, ".avg(),",
    "minimum_value:", col, ".min(),",
    "maximum_value:", col, ".max(),",
    "number_of_rows:", col, ".count()"
  )
  response <- GET(query)
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  results_list[[col]] <- data
}

# Combine all outputs of every column into one data frame
numeric_descriptives <- do.call(rbind, lapply(names(results_list), function(name) {
  df <- as.data.frame(results_list[[name]])
  df$variable <- name
  df
})) %>%
  select(variable, everything()) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# Create a LaTeX table for our report
latex_table <- kable(
  numeric_descriptives,
  format = "latex",
  booktabs = TRUE,
  caption = "Numeric Descriptive Statistics of Selected \\texttt{ais\\_static} Columns",
  col.names = c("Variable", "Average", "Min", "Max", "Count")
) %>%
  kable_styling(latex_options = "HOLD_position", font_size = 9)

print(latex_table)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Query the API to output the 5 most occuring categories of each character variable, NaN Values are not included
columns_char <- c("name", "call_sign", "flag", "ship_type", "destination")
# Loop through every attribute
for (col in columns_char){
  query <- paste0(base_url, "/ais_static?select=count(),", col, "&order=count.desc&limit=5&", col, "=not.is.null")
  response <- GET(query)
  overview_char_cols <- fromJSON(content(response, "text", encoding = "UTF-8")) 
  print(overview_char_cols)
  
  # Create a LaTeX table for our report
  colnames(overview_char_cols) <- c("Count", "Value")
  
  latex_char <- kable(overview_char_cols, format = "latex", booktabs = TRUE,
                      caption = paste0("Top 5 Values of \\texttt{", col, "}"),
                      col.names = c("Count", col)) %>%
    kable_styling(latex_options = "HOLD_position", font_size = 9)
  print(latex_char)
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Query the API to output the min and max of time based attributes
columns_time <- c("eta", "static_updated_at")
# Loop through every attribute
for (col in columns_time){
  query <- paste0(base_url, "/ais_static?select=min_value:", col, ".min(),max_value:", col, ".max()")
  response <- GET(query)
  min_max_columns_time <- fromJSON(content(response, "text", encoding = "UTF-8")) 
  print(min_max_columns_time)
}

# Query the API to output the most occuring time value of the time based attributes
# Loop through every attribute
for (col in columns_time){
  query <- paste0(base_url, "/ais_static?select=count(),", col, "&order=count.desc&limit=10&", col, "=not.is.null")
  response <- GET(query)
  most_occuring_time <- fromJSON(content(response, "text", encoding = "UTF-8")) 
  print(most_occuring_time)
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Query the API to output the number of missing values of every attribute 
columns_all <- c("mmsi", "imo", "draught", "ship_type_code", "length", "width",
                 "name", "call_sign", "flag", "ship_type", "destination", "eta", "static_updated_at")

# Initialize named vector
missing_counts <- numeric(length(columns_all))
names(missing_counts) <- columns_all

# Loop over columns and fill missing counts
for (col in columns_all) {
  query <- paste0(base_url, "/ais_static?select=count()&", col, "=is.null")
  response <- GET(query)
  result <- fromJSON(content(response, "text", encoding = "UTF-8"))
  missing_counts[col] <- result[[1]]
}

# Convert to single-column data frame with row names
null_counts_df <- data.frame(`Missing Values` = missing_counts)
print(null_counts_df)

# Create a LaTeX table for our report
latex_nulls <- kable(null_counts_df, format = "latex", booktabs = TRUE,
                     caption = "Number of Missings in \\texttt{ais\\_static}",
                     col.names = c("Column", "Missing Values")) %>%
  kable_styling(latex_options = "HOLD_position", font_size = 9)
print(latex_nulls)