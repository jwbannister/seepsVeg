build_auc_stack_2 <- function(lai_stack=NULL, year_index=NULL){
  auc_raster <- raster::stack()
  for (k in year_index){
    year_raster <- lai_stack[[1]] * 0
    auc_raster <- raster::stack(auc_raster, year_raster)
  }
  names(auc_raster)[1:raster::nlayers(auc_raster)] <- paste0("y", year_index)
  pb <- txtProgressBar(min=0, max=raster::ncell(lai_stack), width=80, style=3)
  for (i in 1:raster::ncell(lai_stack)){
    print(i)
    lai_vert <- drill_down(layers=dates_index, stack=lai_stack, point=i)
    colnames(lai_vert) <- c("date", "lai")
    lai_vert <- dplyr::arrange(lai_vert, date)
    lai_vert$day <- lubridate::yday(lai_vert$date)
    lai_vert$year <- lubridate::year(lai_vert$date)
    lai_vert <- lai_vert[complete.cases(lai_vert), ]
    for (j in 1:length(year_index)){
      lai_year <- dplyr::filter(lai_vert, year==year_index[j])
      lai_year <- dplyr::select(lai_year, -date, -year)
      if (nrow(lai_year)==0){
        auc_raster[i][j] <- NA
      } else {
          patched_year <- patch_year(data=lai_year, pass_data=lai_vert)
          a <- loess(lai ~ day, data=patched_year)
          b <- data.frame(x=patched_year$day, y=a$fitted)
          auc_year <- MESS::auc(b$x, b$y, type="spline")
          auc_raster[i][j] <- auc_year
      }
    }
    setTxtProgressBar(pb, i, title="AUC")
  }
  close(pb)
  auc_raster
}

patch_year <- function(data=NULL, pass_data=NULL){
  bottom_patches <- if (min(data$day) > 8) seq(1, min(data$day), 8) else c()
  top_patches <- if (max(data$day) < 357) seq(max(data$day)+8, 365, 8) else c()
  patches <- data.frame(day=c(bottom_patches, top_patches))
    if (length(patches$day) != 0){
      patches <- get_patch_data(data=pass_data, patches=patches)
      patched_year <- dplyr::arrange(rbind(data, patches), day)
    } else{
        patched_year <- dplyr::arrange(data, day)
    }
  patched_year
}

#' Build get data to patch missing offseason readings.
#'
#' \code{get_patch_data} averages LAI data from surrounding years to get data
#'  to replace missing data for a single pixel.
#'
#'  @param data Data frame. Vertical LAI data for the point.
#'  @param patches Data frame. Two column data frame: 1st column is list of
#'    dates where patch is needed, 2nd is those dates normalized by
#'    normalize_dates().
#'  @return Returns data frame with patch LAI values for required dates.
get_patch_data <- function(data=NULL, patches=NULL){
  for (i in 1:nrow(patches)){
    d <- patches$day[i]
    tmp <- dplyr::filter(data, (day > d - 7) & (day < d + 7))
    val <- mean(tmp$lai, na.rm=TRUE)
    patches$lai[i] <- val
  }
  patches
}
