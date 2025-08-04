# Helper to fetch AIS data for a given MMSI from PostgREST
get_ais_by_mmsi <- function(mmsi, base_url = "https://aidaho-edu.uni-hohenheim.de/aisdb/ais_dynamic"
                            ) {
  url <- sprintf("%s?mmsi=eq.%s", base_url, mmsi)
  res <- GET(url)
  data <- fromJSON(content(res, "text"), flatten = TRUE)
  return(data)
}

df <- get_ais_by_mmsi(412420898)
head(df)