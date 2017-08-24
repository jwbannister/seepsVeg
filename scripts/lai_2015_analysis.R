devtools::load_all()
path <- "~/airsci/owens/seeps_springs/aug_2017_analysis/"
lai_df <- load_dbfs(paste0(path, "table_2015.dbf"))
colnames(lai_df) <- tolower(colnames(lai_df))
lai_df[lai_df==-9999] <- NA
colnames(lai_df)[2:ncol(lai_df)] <- substring(colnames(lai_df)[2:ncol(lai_df)], 2)

split_lai <- split_df(lai_df, 100)
split_lai <- lapply(split_lai, reshape2::melt, id.vars=c("feid"),
                     variable.name="date", value.name="lai")

pb <- txtProgressBar(min=0, max=length(split_lai), style=3, width=80)
for (i in 1:length(split_lai)){
  split_lai[[i]]$date <- as.Date(split_lai[[i]]$date, "%Y%m%d")
  split_lai[[i]]$day <- lubridate::yday(split_lai[[i]]$date)
  split_lai[[i]]$year <- lubridate::year(split_lai[[i]]$date)
  split_lai[[i]] <- dplyr::select(split_lai[[i]], -date)
  setTxtProgressBar(pb, i)
}
close(pb)

dates_index <- build_dates_index(data=dplyr::select(lai_df, -feid))
year_index <- unique(lubridate::year(dates_index))

save(dates_index, year_index, file=paste0(path, "indices.RData"))
save(split_lai, file=paste0(path, "lai.RData"))

