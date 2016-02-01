# METADATA AND LIBRARIES -------------------------------------------------------

# snowpack data for area around Owens Lake
# JWB 05/12/2015

library(dplyr)
library(ggplot2)
library(data.table)

# DATA IMPORT ------------------------------------------------------------------
snow_locs <- read.csv("../data/2015 - SnowpackData/OwensSnowDataLocations.csv")
snow_data <- read.csv('../data/2015 - SnowpackData/OwensValleySnowData1984-2014.csv')

colnames(snow_locs) <- tolower(colnames(snow_locs))
renameSnowDataColumns <-function(){
  colnames(snow_data) <- tolower(colnames(snow_data))
  setnames(snow_data, 'station', 'station.id')
  setnames(snow_data, 'station_name', 'station.name')
  setnames(snow_data, 'emxp', 'max.daily.precip')
  setnames(snow_data, 'missing', 'max.daily.precip.missing')
  setnames(snow_data, 'consecutive.missing', 'max.daily.precip.consec.missing')
  setnames(snow_data, 'mxsd', 'max.snow.depth')
  setnames(snow_data, 'missing.1', 'max.snow.depth.missing')
  setnames(snow_data, 'consecutive.missing.1', 'max.snow.depth.consec.missing')
  setnames(snow_data, 'tpcp', 'total.precip')
  setnames(snow_data, 'missing.2', 'total.precip.missing')
  setnames(snow_data, 'consecutive.missing.2', 'total.precip.consec.missing')
  setnames(snow_data, 'tsnw', 'total.snow')
  setnames(snow_data, 'missing.3', 'total.snow.missing')
  setnames(snow_data, 'consecutive.missing.3', 'total.snow.consec.missing')
  setnames(snow_data, 'emxt', 'max.temp')
  setnames(snow_data, 'missing.4', 'max.temp.missing')
  setnames(snow_data, 'consecutive.missing.4', 'max.temp.consec.missing')
  setnames(snow_data, 'emnt', 'min.temp')
  setnames(snow_data, 'missing.5', 'min.temp.missing')
  setnames(snow_data, 'consecutive.missing.5', 'min.temp.consec.missing')
  setnames(snow_data, 'mmxt', 'mean.high.temp')
  setnames(snow_data, 'missing.6', 'mean.high.temp.missing')
  setnames(snow_data, 'consecutive.missing.6', 'mean.high.temp.consec.missing')
  setnames(snow_data, 'mmnt', 'mean.low.temp')
  setnames(snow_data, 'missing.7', 'mean.low.temp.missing')
  setnames(snow_data, 'consecutive.missing.7', 'mean.low.temp.consec.missing')
  setnames(snow_data, 'mntm', 'mean.temp')
  setnames(snow_data, 'missing.8', 'mean.temp.missing')
  setnames(snow_data, 'consecutive.missing.8', 'mean.temp.consec.missing')
  snow_data
}
snow_data <- renameSnowDataColumns()
for (i in 1:length(levels(snow_data$station.name))){
  levels(snow_data$station.name)[i] <- substr(levels(snow_data$station.name)[i], 1, nchar(as.character(levels(snow_data$station.name)[i])) - 5)
}

swedes_loc <- list(latitude=99999, longitude=99999)

# EXPLORATORY ----------------------------------------------------------------

# get only stations near to Owens Lake
getCloseStations <- function(){
  b <- as.character(snow_locs$station.name)
  snow_owens <- filter(snow_data, grepl(b[1], station.name))
  for (i in 2:length(b)){
    snow_owens <- rbind(snow_owens, filter(snow_data, grepl(b[i], station.name)))
  }
  snow_owens
}
snow_owens <- getCloseStations()
snow_owens$station.name <- factor(snow_owens$station.name)
snow_owens$station.id <- factor(snow_owens$station.id)

# remove lat, long, and elev columns from snow_owens (contained in snow_locs data table)
snow_owens <- select(snow_owens, -elevation, -latitude, -longitude)

# deal with missing data (-9999 values)
for (i in c(4, 7, 10, 13, 16, 19, 22, 25, 28)){
  for (j in 1:nrow(snow_owens)){
    if (snow_owens[j, i]==-9999){
      snow_owens[j, i] = NA
    }
  }
}

snow_owens$year <- as.numeric(substr(as.character(snow_owens$date), 1, 4))
total_precip <- snow_owens %>% group_by(station.name, year) %>%  summarise(sum=sum(as.numeric(total.precip)))

class(snow_owens$total.precip[1])
as.numeric(snow_owens$total.precip)
