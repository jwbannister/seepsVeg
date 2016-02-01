#' Build HDF5 file.
#'
#' \code{build_hdf} builds an HDF5 file to store LAI data for Owens Lake
#'  Seeps and Springs areas.
#'
#'  @param file Character string. HDF5 file to be created, including path. File
#'    must have ".h5" extension.
#'  @param data Data frame. Should include all applicable areas in a "zone"
#'    column.
#'  @return Creates HDF5 file at specified location.
build_hdf <- function(file=NULL, data=NULL){
  rhdf5::h5createFile(file)
  for (i in levels(data$zone)){
    rhdf5::h5createGroup(file, i)
    rhdf5::h5createGroup(file, paste0(i, "/lai"))
    rhdf5::h5createGroup(file, paste0(i, "/lai/images"))
    rhdf5::h5createGroup(file, paste0(i, "/lai/models"))
    rhdf5::h5createGroup(file, paste0(i, "/auc"))
    rhdf5::h5createGroup(file, paste0(i, "/auc/images"))
    rhdf5::h5createGroup(file, paste0(i, "/auc/models"))
  }
}

write_hdf_point_grids <- function(data=NULL, file=NULL){
  for (i in unique(data$zone)){
    point_grid <- build_area_point_grid(area=i, data=data)
    rhdf5::h5write(point_m, file, paste0(i, "/point_grid"))
  }
}

#' Write to Air Sciences PostgreSQL database.
#'
#' \code{write_airscidb} writes from a dataframe into an existing table in the
#'  Air Sciences PostgreSQL database "owenslake" ("springs" schema) at
#'  db.airsci.com.
#'
#'  @param data Dataframe. Data to be written.
#'  @param tble Character string. Table in the "springs" schema in the
#'    "owenslake" database to be wrtitten to.
write_airscidb <- function(data=NULL, tble=NULL){
  drv <- DBI::dbDriver("PostgreSQL")
  con <- DBI::dbConnect(drv, dbname="owenslake", host="db.airsci.com", user="airsci",
                        password="g!0b@!w@rming")
  DBI::dbExistsTable(con, c("springs", tble))
  DBI::dbWriteTable(con, c("springs", tble), value=data, append=TRUE,
                    row.names=FALSE)
  DBI::dbDisconnect(con)
}

split_write_airscidb <- function(){
  drv <- DBI::dbDriver("PostgreSQL")
  con <- DBI::dbConnect(drv, dbname="owenslake", host="db.airsci.com", user="airsci",
                        password="g!0b@!w@rming")
  pb <- txtProgressBar(min=0, max=nrow(lai_melt), style=3, width=80)
  for (i in 1:nrow(lai_melt)){
    data <- lai_melt[i, ]
    DBI::dbWriteTable(con, c("springs", "lai"), value=data, append=TRUE,
                      row.names=FALSE)
    setTxtProgressBar(pb, i)
  }
  close(pb)
  DBI::dbDisconnect(con)
}
