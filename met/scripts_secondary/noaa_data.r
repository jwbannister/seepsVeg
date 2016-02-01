library(ggmap)
library(dplyr)
library(jsonlite)
library(ggplot2)
library(httr)
library(ggvis)

# functions ----------------------------
parseNOAA <- function(x){
  # this function is intended to parse output data from NOAA web API calls
  parsed <- content(x, as="parsed", type="application/json")$results
  headers <- names(parsed[[1]])
  df <- data.frame(matrix(unlist(parsed), ncol=length(headers), byrow=TRUE, 
                          dimnames=list(seq(1, length(parsed), 1), headers)),
                   stringsAsFactors=FALSE)
  df
}

makeCSVBackup <- function(x){
  # this function will write a csv file as a backup for a data frame
  # the file will be written in a "data" directory at the same level as the 
  # working directory. File location is based on JWB airsci project file structure,
  # directory is written in Linux format.
  nm <- deparse(substitute(x))
  dt <- gsub("-", "_", Sys.Date())
  filenm <- paste0("../data/", nm, "_", dt, ".csv")
  write.csv(x, file=filenm)
}

# NOAA web services API for land stations------------------------------
# https://www.ncdc.noaa.gov/isd
token_key <- "VlrojpyMyUPtVaFyszrYixWTsBoArGDI"

# find NOAA stations around Owens Lake
extent <- "36,-118.5,37,-117.3"
stations_url <- paste0("http://www.ncdc.noaa.gov/cdo-web/api/v2/stations?extent=", extent)
noaa_stations <- GET(stations_url, add_headers(token=token_key))

noaa_stations_df <- parseNOAA(noaa_stations)

# format and class data frame columns
noaa_stations_df$longitude <- as.numeric(noaa_stations_df$longitude)
noaa_stations_df$latitude <- as.numeric(noaa_stations_df$latitude)
noaa_stations_df$elevation <- as.numeric(noaa_stations_df$elevation)
noaa_stations_df$mindate <- as.Date(noaa_stations_df$mindate)
noaa_stations_df$maxdate <- as.Date(noaa_stations_df$maxdate)
noaa_stations_df$datacoverage <- as.numeric(noaa_stations_df$datacoverage)

# map locations of collected NOAA weather stations
bounds = make_bbox(noaa_stations_df$longitude, noaa_stations_df$latitude)
owens_map <- get_map(location=bounds, maptype="satellite", source="google", color="color", zoom=9)
ggmap(owens_map) +
  geom_point(data=noaa_stations_df, aes(x=longitude, y=latitude), color="red")

# what datasets are available for a selection of NOAA weather stations
findAvailableData <- function(x=noaa_stations_df, tok=token_key){
  # this function will return a data frame listing available data sets from a 
  # list of NOAA weather stations. x should be a data frame of NOAA weather
  # stations as returned from the function parseNOAA.
  for (i in 1:nrow(x)){
    id <- x$id[i]
    url <- paste0("http://www.ncdc.noaa.gov/cdo-web/api/v2/datasets?stationid=", id)
    sets <- GET(url, add_headers(token=tok))
    sets_df <- parseNOAA(sets)
    sets_df$stationid <- id
    sets_df$stationname <- x$name[i]
    if (i==1) total_df <- sets_df else total_df <- rbind(total_df, sets_df)
  }
  row.names(total_df) <- seq(1, nrow(total_df), 1)
  total_df
}

avail_data_df <- findAvailableData()
summary_stations <- filter(avail_data_df, id=="GHCNDMS")

# get information about available data categories for the stations
station_id <- paste0("stationid=", paste(summary_stations$stationid, collapse="&stationid="))
cat_url <- paste0("http://www.ncdc.noaa.gov/cdo-web/api/v2/datacategories?", station_id)
categories <- GET(cat_url, add_headers(token=token_key))
cats_df <- parseNOAA(categories)

# get information about available data types in a data category
datatype_id <- "PRCP"
type_url <- paste0("http://www.ncdc.noaa.gov/cdo-web/api/v2/datatypes?datacategoryid=", datatype_id)
types <- GET(type_url, add_headers(token=token_key))
types_df <- parseNOAA(types)

# get the NOAA data we are interested in
for (i in summary_stations$stationid){
  station_id <- paste0("&stationid=", i)
  dataset_id <- "datasetid=GHCNDMS"
  start_date <- "&startdate=2005-01-01"
  end_date <- "&enddate=2014-12-31"
  data_url <- paste0("http://www.ncdc.noaa.gov/cdo-web/api/v2/data?", dataset_id, station_id,
                   start_date, end_date, "&limit=1000")
  noaa_data <- GET(data_url, add_headers(token=token_key))

  test_parse <- content(noaa_data, as="parsed", type="application/json")$results



