devtools::load_all()

# ALL DATA-RAW FILES NOW KEPT ON DROPBOX
# load functions must be changed to access files off dropbox
# files too large to version control with github

feid_spatial <- rgdal::readOGR("../data_raw/FEID_Locations.gdb",
                               "SS_extent_30mGrid_v2_WGS84_centroid_subset")
feid_df <- data.frame(feid=feid_spatial$FEID, x=feid_spatial@coords[ , 1],
                      y=feid_spatial@coords[ , 2])

zones_df <- foreign::read.dbf(file="../data_raw/FEID_and_ZoneName.dbf")
colnames(zones_df) <- tolower(colnames(zones_df))

points_df <- dplyr::left_join(feid_df, dplyr::select(zones_df, -objectid), by="feid")
levels(points_df$zone) <- tolower(gsub(" ", "_", levels(points_df$zone)))

data_years <- c(1984:2014)
files <- c()
for (i in data_years){
  nxt <- paste0("../data_raw/table_",
                as.character(i), ".dbf")
  files <- c(files, nxt)
}

lai_df <- load_dbfs(files)
colnames(lai_df) <- tolower(colnames(lai_df))
lai_df[lai_df==-9999] <- NA

owens_lai <- dplyr::inner_join(points_df, lai_df, by="feid")
owens_lai$x <- round(owens_lai$x, 1)
owens_lai$y <- round(owens_lai$y, 1)
save(owens_lai, file="../data/owens_lai.RData")

dates_index <- build_dates_index(data=owens_lai)
year_index <- unique(lubridate::year(dates_index))
zone_index <- levels(owens_lai$zone)

save(dates_index, year_index, zone_index, file="../data/owens_index.RData")
