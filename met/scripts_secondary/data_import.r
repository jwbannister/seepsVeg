# METADATA AND LIBRARIES -------------------------------------------------------

# input climate data for area around Owens Lake
# JWB 05/12/2015

library(dplyr)
library(ggplot2)
library(data.table)

# DATA IMPORT ------------------------------------------------------------------

cimis_input <- read.csv("../data/cimis/owens_cimis_monthly.csv")
cimis_df <- data.frame("station.name"=cimis_input$Stn.Name, 
                       "month.year"=cimis_input$Month.Year, 
                       "total.precip"=cimis_input$Total.Precip..in.,
                       "total.precip.qc"=cimis_input$qc.1, 
                       "avg.air.temp"=cimis_input$Avg.Air.Temp..F.,
                       "avg.air.temp.qc"=cimis_input$qc.6, 
                       "avg.soil.temp"=cimis_input$Avg.Soil.Temp..F.,
                       "avg.soil.temp.qc"=cimis_input$qc.12)

noaa_input <- read.csv("../data/noaa/535065.csv")

renameNoaaDataColumns <-function(){
  colnames(noaa_input) <- tolower(colnames(noaa_input))
  setnames(noaa_input, 'station', 'station.id')
  setnames(noaa_input, 'station_name', 'station.name')
  setnames(noaa_input, 'emxp', 'max.daily.precip')
  setnames(noaa_input, 'missing', 'max.daily.precip.missing')
  setnames(noaa_input, 'consecutive.missing', 'max.daily.precip.consec.missing')
  setnames(noaa_input, 'mxsd', 'max.snow.depth')
  setnames(noaa_input, 'missing.1', 'max.snow.depth.missing')
  setnames(noaa_input, 'consecutive.missing.1', 'max.snow.depth.consec.missing')
  setnames(noaa_input, 'tpcp', 'total.precip')
  setnames(noaa_input, 'missing.2', 'total.precip.missing')
  setnames(noaa_input, 'consecutive.missing.2', 'total.precip.consec.missing')
  setnames(noaa_input, 'tsnw', 'total.snow')
  setnames(noaa_input, 'missing.3', 'total.snow.missing')
  setnames(noaa_input, 'consecutive.missing.3', 'total.snow.consec.missing')
  setnames(noaa_input, 'emxt', 'max.temp')
  setnames(noaa_input, 'missing.4', 'max.temp.missing')
  setnames(noaa_input, 'consecutive.missing.4', 'max.temp.consec.missing')
  setnames(noaa_input, 'emnt', 'min.temp')
  setnames(noaa_input, 'missing.5', 'min.temp.missing')
  setnames(noaa_input, 'consecutive.missing.5', 'min.temp.consec.missing')
  setnames(noaa_input, 'mmxt', 'mean.high.temp')
  setnames(noaa_input, 'missing.6', 'mean.high.temp.missing')
  setnames(noaa_input, 'consecutive.missing.6', 'mean.high.temp.consec.missing')
  setnames(noaa_input, 'mmnt', 'mean.low.temp')
  setnames(noaa_input, 'missing.7', 'mean.low.temp.missing')
  setnames(noaa_input, 'consecutive.missing.7', 'mean.low.temp.consec.missing')
  setnames(noaa_input, 'mntm', 'mean.temp')
  setnames(noaa_input, 'missing.8', 'mean.temp.missing')
  setnames(noaa_input, 'consecutive.missing.8', 'mean.temp.consec.missing')
  noaa_input
}
noaa_df <- renameNoaaDataColumns()

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
snow_owens$year <- as.numeric(substr(as.character(snow_owens$date), 1, 4))

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


yearly_data <- snow_owens %>% 
  group_by(station.name, year) %>%  
  summarise(total.precip=sum(as.numeric(total.precip)), )

class(snow_owens$total.precip[1])
as.numeric(snow_owens$total.precip)
