devtools::load_all()

# ALL DATA-RAW NOW STORED ON DROPBOX!
# load functions must be changed to access raw data files from dropbox.
# files too large to version control with github.

feid_spatial <- rgdal::readOGR("./data-raw/FEID_Locations.gdb",
                               "SS_extent_30mGrid_v2_WGS84_centroid_subset")
feid_df <- data.frame(feid=feid_spatial$FEID, x=feid_spatial@coords[ , 1],
                      y=feid_spatial@coords[ , 2])

zones_df <- foreign::read.dbf(file="./data-raw/FEID_and_ZoneName.dbf")
colnames(zones_df) <- tolower(colnames(zones_df))

pixels_df <- dplyr::left_join(feid_df, dplyr::select(zones_df, -objectid), by="feid")
pixels_df$x <- round(pixels_df$x, 1)
pixels_df$y <- round(pixels_df$y, 1)

data_years <- c(1984:2014)
files <- c()
for (i in data_years[29]){
  nxt <- paste0("./data-raw/yearly_lai/table_",
                as.character(i), ".dbf")
  files <- c(files, nxt)
}
lai_df <- load_dbfs(files)
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
zone_index <- levels(lai_df$zone)

save(dates_index, year_index, zone_index, file="./data/indices.RData")
save(pixels_df, file="./data-analysis/pixels.RData")
save(split_lai, file="./data-analysis/lai.RData")
