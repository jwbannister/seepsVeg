# read in and clean CIMIS data for Owens stations

library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)

# CIMIS Owens North (183) daily 
cimis_north_daily <- read.csv("../data/cimis/cimis_north_daily.csv")
cimis_north_daily <- select(cimis_north_daily, date=Date, precip=Precip..in.,
                            qc.precip=qc.1, soil.temp=Avg.Soil.Temp..F., 
                            qc.soil=qc.13)
cimis_north_daily$date <- as.Date(as.character(cimis_north_daily$date), "%m/%d/%Y")
cimis_north_daily <- cimis_north_daily[!is.na(cimis_north_daily$date), ]
cimis_north_daily <- cimis_north_daily[year(cimis_north_daily$date)!=2002, ]
cimis_north_daily$station <- "183"

# CIMIS Owens South (189) daily 
cimis_south_daily <- read.csv("../data/cimis/cimis_south_daily.csv")
cimis_south_daily <- select(cimis_south_daily, date=Date, precip=Precip..in.,
                            qc.precip=qc.1, soil.temp=Avg.Soil.Temp..F., 
                            qc.soil=qc.13)
cimis_south_daily$date <- as.Date(as.character(cimis_south_daily$date), "%m/%d/%Y")
cimis_south_daily <- cimis_south_daily[!is.na(cimis_south_daily$date), ]
cimis_south_daily$station <- "189"

cimis_daily <- rbind(cimis_north_daily, cimis_south_daily)
cimis_precip <- filter(select(cimis_daily, date, station, precip, qc.precip), !is.na(precip))

start_date <- min(cimis_precip[duplicated(cimis_precip$date), ]$date)
cimis_precip <- filter(cimis_precip, date >= start_date)

cimis_precip <- cimis_precip %>% group_by(station) %>% arrange(date) %>%
  mutate(running30.avg=NA, running365.avg=NA, delta=precip-mean(precip), cum.delta=0)

pb <- txtProgressBar(min=0, max=3 * nrow(cimis_precip), style=3, width=80)
tick <- 0
for (i in unique(cimis_precip$station)){
  for (j in 2:nrow(cimis_precip[cimis_precip$station==i, ])){
    cimis_precip[cimis_precip$station==i, ]$cum.delta[j] <- 
      sum(cimis_precip[cimis_precip$station==i, ]$delta[1:j])
    tick <- tick + 1
    setTxtProgressBar(pb, tick)
  }
  for (k in 15:(nrow(cimis_precip[cimis_precip$station==i, ])-15)){
    cimis_precip[cimis_precip$station==i, ]$running30.avg[k] <- 
      sum(cimis_precip[cimis_precip$station==i, ]$precip[(k-14):(k+15)])/30
    tick <- tick + 1
    setTxtProgressBar(pb, tick)
  }
  for (m in 183:(nrow(cimis_precip[cimis_precip$station==i, ])-182)){
    cimis_precip[cimis_precip$station==i, ]$running365.avg[m] <- 
      sum(cimis_precip[cimis_precip$station==i, ]$precip[(m-182):(m+182)])/365
    tick <- tick + 1
    setTxtProgressBar(pb, tick)
  }
}
close(pb)

# Monthly data
cimis_precip_monthly <- cimis_precip %>% group_by(station, year=year(date), month=month(date)) %>% 
  summarize(precip.calendar.month=sum(precip), ref.date=mean(date))
cimis_precip_monthly <- cimis_precip_monthly %>% ungroup() %>% group_by(station) %>%
  mutate(delta=precip.calendar.month - mean(precip.calendar.month), cum.delta=0)
for (i in unique(cimis_precip_monthly$station)){
  for (j in 2:nrow(cimis_precip_monthly[cimis_precip_monthly$station==i, ])){
    cimis_precip_monthly[cimis_precip_monthly$station==i, ]$cum.delta[j] <- 
      sum(cimis_precip_monthly[cimis_precip_monthly$station==i, ]$delta[1:j])
  }
}

cimis_precip_yearly <- cimis_precip %>% group_by(station, year=year(date)) %>% 
  summarize(precip.calendar.year=sum(precip), ref.date=mean(date))
cimis_precip_yearly <- cimis_precip_yearly %>% ungroup() %>% group_by(station) %>%
  mutate(delta=precip.calendar.year - mean(precip.calendar.year), cum.delta=0)
for (i in unique(cimis_precip_yearly$station)){
  for (j in 2:nrow(cimis_precip_yearly[cimis_precip_yearly$station==i, ])){
    cimis_precip_yearly[cimis_precip_yearly$station==i, ]$cum.delta[j] <- 
      sum(cimis_precip_yearly[cimis_precip_yearly$station==i, ]$delta[1:j])
  }
}
# 2003 does not have a full year of data available
cimis_precip_yearly <- filter(cimis_precip_yearly, year!=2003)

# Soil
cimis_soil <- filter(select(cimis_daily, date, station, soil.temp, qc.soil), !is.na(soil.temp))
cimis_soil <- filter(cimis_soil, date >= start_date)
cimis_soil <- cimis_soil[cimis_soil$soil.temp!="", ]
cimis_soil <- cimis_soil[cimis_soil$soil.temp!="R", ]
cimis_soil$soil.temp <- as.numeric(cimis_soil$soil.temp)
cimis_soil <- cimis_soil %>% group_by(station) %>% arrange(date) %>%
  mutate(delta=soil.temp - mean(soil.temp))
cimis_soil_monthly <- cimis_soil %>% ungroup() %>% mutate(year=year(date), month=month(date)) %>%
  group_by(station, year, month) %>% summarize(mean.temp=mean(soil.temp), degreedays.over.avg=sum(delta), 
                                               ref.date=mean(date))
cimis_soil_yearly <- cimis_soil %>% ungroup() %>% mutate(year=year(date)) %>%
  group_by(station, year) %>% summarize(mean.temp=mean(soil.temp), degreedays.over.avg=sum(delta), 
                                        ref.date=mean(date))

# save images of cleaned data
save(cimis_precip, cimis_precip_monthly, cimis_precip_yearly, cimis_soil, 
     cimis_soil_monthly, cimis_soil_yearly, file="../output/cimis_data.RData")
