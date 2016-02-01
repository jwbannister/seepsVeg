load("./data/owens_index.RData")
load("./data/backup_10192015/swedes_pasture.RData")
devtools::load_all("~/Dropbox (airsci)/code/lairr")
library(ggplot2)
library(dplyr)
feid <- 562606
ind <- which(points$"point.id"[ , ]==feid)
yr <- 2011

lai_vert <- drill_down(layers=dates_index, stack=lai, point=ind)
colnames(lai_vert) <- c("date", "lai")
lai_vert <- normalize_dates(data=lai_vert)
lai_vert$day <- lubridate::yday(lai_vert$date)
lai_vert$year <- lubridate::year(lai_vert$date)

daily_lai_data <- build_lai_window_data(data=filter(lai_vert, year < yr))

daily_lai_summary <- build_lai_window_summary(data=filter(lai_vert, year < yr))
daily_lai_summary$day <- lubridate::yday(daily_lai_summary$date)

ggplot(lai_vert, aes(x=day, y=lai)) +
  geom_point() +
  geom_smooth(data=daily_lai_summary, mapping=aes(x=day, y=running.avg), color="red", se=FALSE) +
  geom_smooth(data=daily_lai_summary, mapping=aes(x=day, y=running.avg-running.sd), color="red", se=FALSE) +
  geom_smooth(data=daily_lai_summary, mapping=aes(x=day, y=running.avg+running.sd), color="red", se=FALSE)

num.obs <- 25
yr.obs <- filter(lai_vert, year==yr)[1:num.obs, ]
yr.obs <- yr.obs[complete.cases(yr.obs), ]
ggplot(lai_vert, aes(x=day, y=lai)) +
  geom_smooth(data=daily_lai_summary, mapping=aes(x=day, y=running.avg), color="red", se=FALSE) +
  geom_smooth(data=daily_lai_summary, mapping=aes(x=day, y=running.avg-running.sd), color="red", se=FALSE) +
  geom_smooth(data=daily_lai_summary, mapping=aes(x=day, y=running.avg+running.sd), color="red", se=FALSE) +
  geom_point(data=yr.obs, mapping=aes(x=day, y=lai), color="blue", size=4,
             shape=13) +
  geom_path(data=yr.obs, mapping=aes(x=day, y=lai), color="blue")


