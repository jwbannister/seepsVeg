load("../output/cimis_data.RData")

library(gridExtra)
library(reshape2)
library(ggplot2)
library(magrittr)
library(dplyr)

plotDaily <- function(){
  plt1 <- ggplot(cimis_precip, aes(x=date, y=precip)) + 
    ggtitle("CIMIS Station Daily Precip") + ylab("inches") +
    geom_path(aes(color=station)) +
    theme(legend.position = c(.85, .75))
  a <- cimis_precip[duplicated(cimis_precip$date), ]$date
  b <- filter(cimis_precip, date %in% a) %>% dcast (date ~ station, value.var="precip")
  colnames(b)[2:3] <- c("precip.183", "precip.189")  
  plt2 <- ggplot(b, aes(x=precip.183, y=precip.189)) + 
    ggtitle("CIMIS Station Correlation") + 
    xlab("station 183 precip (in)") + ylab("station 189 precip (in)") +
    geom_point() +
    annotate(geom="text", hjust=0, x=0.5, y=1.75, 
             label=paste0("cor. coeff. = ", round(cor(b$precip.183, b$precip.189), 2)))
  grid.arrange(plt1, plt2, ncol=2)
}
plotDaily()

plotRunningAvg <- function(){
  plt1 <- ggplot(cimis_precip, aes(x=date, y=running30.avg)) + 
    ggtitle("CIMIS Station Precip - Running 30 Day Average") + ylab("inches") +
    geom_path(aes(color=station)) +
    theme(legend.position = c(.85, .75))
  plt2 <- ggplot(cimis_precip, aes(x=date, y=running365.avg)) + 
    ggtitle("CIMIS Station Precip - Running 365 Day Average") + ylab("inches") +
    geom_path(aes(color=station)) +
    theme(legend.position = c(.85, .75))
  grid.arrange(plt1, plt2, ncol=2)
}
plotRunningAvg()

plotMonthly <- function(){
  plt1 <- ggplot(cimis_precip_monthly, aes(x=ref.date, y=precip.calendar.month)) + 
    ggtitle("CIMIS Station Monthly Precip") + ylab("inches") +
    geom_path(aes(color=station)) + xlab("date") +
    theme(legend.position = c(.85, .75))
  plt2 <- ggplot(cimis_precip_monthly, aes(x=ref.date, y=cum.delta)) + 
    ggtitle("CIMIS Station Precip - Cumulative Departure (Monthly)") + ylab("inches") +
    geom_path(aes(color=station)) + xlab("date") +
    theme(legend.position = c(.85, .75))
  grid.arrange(plt1, plt2, ncol=2)
}
plotMonthly()

plotYearly <- function(){
  plt1 <- ggplot(cimis_precip_yearly, aes(x=ref.date, y=precip.calendar.year)) + 
    ggtitle("CIMIS Station Yearly Precip") + ylab("inches") +
    geom_path(aes(color=station)) + xlab("date") +
    theme(legend.position = c(.85, .75))
  plt2 <- ggplot(cimis_precip_yearly, aes(x=ref.date, y=cum.delta)) + 
    ggtitle("CIMIS Station Precip - Cumulative Departure (Yearly)") + ylab("inches") +
    geom_path(aes(color=station)) + xlab("date") +
    theme(legend.position = c(.85, .75))
  grid.arrange(plt1, plt2, ncol=2)
}
plotYearly()

plotSoilDaily <- function(){
  plt1 <- ggplot(cimis_soil, aes(x=date, y=soil.temp)) + 
    ggtitle("CIMIS Station Daily Soil Temperature") + ylab("inches") +
    geom_point(aes(color=station)) +
    theme(legend.position = c(.95, .90)) +
    scale_y_discrete(breaks=c(seq(35, 95, 10))) + ylim(35, 95)
  a <- cimis_soil[duplicated(cimis_soil$date), ]$date
  b <- filter(cimis_soil, date %in% a) %>% dcast (date ~ station, value.var="soil.temp")
  colnames(b)[2:3] <- c("soil.temp.183", "soil.temp.189")  
  plt2 <- ggplot(b, aes(x=soil.temp.183, y=soil.temp.189)) + 
    ggtitle("CIMIS Station Correlation") + 
    xlab("station 183 soil temp (F)") + ylab("station 189 soil temp (F)") +
    geom_point() +
    annotate(geom="text", hjust=0, x=50, y=80, 
             label=paste0("cor. coeff. = ", round(cor(b$soil.temp.183, b$soil.temp.189), 2)))
  grid.arrange(plt1, plt2, ncol=2)
}
plotSoilDaily()

plotSoilMonthly <- function(){
  plt1 <- ggplot(cimis_soil_monthly, aes(x=ref.date, y=mean.temp)) + 
    ggtitle("CIMIS Station Monthly Soil Temp") + ylab("degF") +
    geom_path(aes(color=station)) + xlab("date") +
    theme(legend.position = c(.85, .75))
  plt2 <- ggplot(cimis_soil_monthly, aes(x=ref.date, y=degreedays.over.avg)) + 
    ggtitle("CIMIS Station Soil Temp - Cumulative Departure (Monthly)") + ylab("degF-days") +
    geom_path(aes(color=station)) + xlab("date") +
    theme(legend.position = c(.85, .75))
  grid.arrange(plt1, plt2, ncol=2)
}
plotSoilMonthly()

plotSoilYearly <- function(){
  plt1 <- ggplot(cimis_soil_yearly, aes(x=ref.date, y=mean.temp)) + 
    ggtitle("CIMIS Station Yearly Soil Temp") + ylab("degF") +
    geom_path(aes(color=station)) + xlab("date") +
    theme(legend.position = c(.85, .75))
  plt2 <- ggplot(cimis_soil_yearly, aes(x=ref.date, y=degreedays.over.avg)) + 
    ggtitle("CIMIS Station Soil Temp - Cumulative Departure (Yearly)") + ylab("degF-days") +
    geom_path(aes(color=station)) + xlab("date") +
    theme(legend.position = c(.85, .75))
  grid.arrange(plt1, plt2, ncol=2)
}
plotSoilYearly()