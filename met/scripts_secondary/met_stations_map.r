# identify and map meteorological stations around Owens Lake for inclusion in Seeps
# and Springs study

source("functions.r")

library(ggmap)
library(dplyr)
library(jsonlite)
library(ggplot2)
library(httr)

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
avail_data_df <- filter(avail_data_df, id=="GHCND" | id=="GHCNDMS")

colnames(noaa_stations_df)[which(names(noaa_stations_df)=="id")] <- "stationid"
noaa_stations_df<- semi_join(noaa_stations_df, avail_data_df, by="stationid")
noaa_stations_df <- select(noaa_stations_df, name=name, id=stationid, latitude=latitude,
                           longitude=longitude) %>% mutate(agency="NOAA", notes=NA)

#CIMIS Stations --------------------
# see http://www.cimis.water.ca.gov/ for more information on CIMIS stations and 
# available data items. 
cimis_stations_df <- rbind(data_frame(name = "Owens Lake North",
                                      id = 183,
                                      latitude = 36.488611,
                                      longitude = -117.91944,
                                      notes= "precipitaion and soil temperature (daily)"),
                           data_frame(name = "Owens Lake South",
                                      id = 189,
                                      latitude = 36.358628,
                                      longitude = -117.94387,
                                      notes= "precipitaion and soil temperature (daily)"))
cimis_stations_df <- mutate(cimis_stations_df, agency="CIMIS")
cimis_stations_df <- cimis_stations_df %>% select(name, id, latitude, longitude, agency, notes)

# CDEC (CA Dept. of Water Resources) Stations -------------------
# see http://cdec.water.ca.gov/
cdec_stations_df <- rbind(data_frame(name = "Cottonwood Lakes",
                                     id = "CWD",
                                     latitude = 36.48383,
                                     longitude = -118.17755,
                                     notes= "precipitation (daily). Data a mess, not needed (will use CTT data)."),
                          data_frame(name = "Owens River - Below Tinemaha",
                                     id = "OTR",
                                     latitude = 37.058,
                                     longitude = -118.225,
                                     notes= "river flow (monthly)"),
                          data_frame(name = "Tinemaha Reservoir",
                                     id = "TNM",
                                     latitude = 37.058,
                                     longitude = -118.225,
                                     notes= "reservoir storage (monthly)"),
                          data_frame(name = "Haiwee",
                                     id = "HWE",
                                     latitude = 36.137,
                                     longitude = -117.948,
                                     notes= "precipitation (monthly)"),
                          data_frame(name = "Cottonwood Gates",
                                     id = "CTT",
                                     latitude = 36.417,
                                     longitude = -118.033,
                                     notes= "precipitation (monthly)"),
                          data_frame(name = "Trailhead",
                                     id = "TRL",
                                     latitude = 36.337,
                                     longitude = -118.155,
                                     notes= "snow depth and snow water content (monthly)"),
                          data_frame(name = "Cottonwood Lakes 1",
                                     id = "CW1",
                                     latitude = 36.483,
                                     longitude = -118.177,
                                     notes= "snow depth and snow water content (monthly)"),
                          data_frame(name = "Cottonwood Lakes 2",
                                     id = "CW2",
                                     latitude = 36.483,
                                     longitude = -118.217,
                                     notes= "snow depth and snow water content (monthly)"),
                          data_frame(name = "Independence",
                                     id = "IPN",
                                     latitude = 36.798,
                                     longitude = -118.204,
                                     notes= " precipitation (monthly)"))
cdec_stations_df <- mutate(cdec_stations_df, agency="CDEC")
cdec_stations_df <- cdec_stations_df %>% select(name, id, latitude, longitude, agency, notes)

# Combine stations -------------------
met_stations <- rbind(noaa_stations_df, cimis_stations_df, cdec_stations_df)
met_stations[met_stations$agency=="NOAA", ]$notes <- "better location available / data available elsewhere"
met_stations[grepl("HUNTER", met_stations$name), ]$notes <- "only temp data available, no precip"
met_stations[met_stations$id=="OTR", ]$notes <- paste0(met_stations[met_stations$id=="OTR", ]$notes, ". No data available.")

save(met_stations, file="../output/station_data.RData")
write.csv(met_stations, file="../output/station_table.csv")

# map locations
bounds = make_bbox(met_stations$longitude, met_stations$latitude)
owens_map <- get_map(location=bounds, maptype="satellite", source="google", color="color", zoom=9)
p1 <- ggmap(owens_map) + 
   geom_point(data=met_stations, aes(x=longitude, y=latitude, color=agency), size=4) +
  ggtitle("Available Ground Stations")
makePNG(p1, "all_stations_map")

good_stations <- met_stations %>% filter(agency!="NOAA", id!="OTR" | id!="CWD") 
bounds = make_bbox(good_stations$longitude, good_stations$latitude)
owens_map <- get_map(location=bounds, maptype="satellite", source="google", color="color", zoom=9)
p2 <- ggmap(owens_map) + 
  geom_point(data=good_stations, aes(x=longitude, y=latitude, color=agency), size=4) +
  ggtitle("Useable Ground Stations")
makePNG(p2, "good_stations_map")
