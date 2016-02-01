#' Build a stack of LAI values.
#'
#' \code{build_lai_stack} builds a raster stack of LAI images for a
#'  Seeps and Springs zone.
#'
#'  @param area Character string. Name of Seeps and Springs area.
#'  @return Returns raster stack. Layers have values = LAI reading from that
#'  image.
build_lai_stack <- function(area=NULL){
  ticks <- ncol(owens_lai) - 4
  pb <- txtProgressBar(min=0, max=ticks, width=80, style=3)
  temp <- build_area_point_grid(area=area, data=owens_lai)
  raster_stack <- raster::stack()
  for (j in 5:ncol(owens_lai)){
    single_image <- dplyr::select(temp, 1, 2, j)
    lai_raster <- create_raster(single_image)
    raster_stack <- raster::stack(raster_stack, lai_raster)
    setTxtProgressBar(pb, j - 4, title="LAI")
  }
  close(pb)
  raster::projection(raster_stack) <- "+proj=utm +zone=11 +datum=WGS84"
  raster_stack
}

#' Build a stack of points and locations.
#'
#' \code{build_points_stack} builds a raster stack of Landsat image points
#'  for a Seeps and Springs zone.
#'
#'  @param area Character string. Name of seeps and springs area.
#'  @return Returns raster stack with 2 layers. First layer value = point.id,
#'    second layer value is a number indicating the area if point in defined
#'    zone, 0 if outside.
build_points_stack <- function(area=NULL){
  temp <- build_area_point_grid(area=area, data=owens_lai)
  temp_id <- dplyr::select(temp, x, y, feid)
  temp_zone <- dplyr::select(temp, x, y, zone)
  id_raster <- create_raster(temp_id)
  zone_raster <- create_raster(temp_zone)
  raster_stack <- raster::stack(id_raster, zone_raster)
  raster::projection(raster_stack) <- "+proj=utm +zone=11 +datum=WGS84"
  raster_stack
}

#' Build an index of dates for which LAI images are available.
#'
#' \code{build_dates_index} looks through LAI data in the format as supplied
#'  for Owens Lake and builds a list of all the dates for which LAI images
#'  were obtained.
#'
#'  @param data Data frame. LAI data.
#'  @return Returns a vector of dates.
build_dates_index <- function(data=owens_lai){
  temp <- colnames(owens_lai)[5:ncol(owens_lai)]
  temp <- gsub("d", "", temp)
  temp <- as.Date(temp, "%Y%m%d")
  temp
}

#' Normalize all dates to a single year.
#'
#' \code{normalize_dates} changes the year of all dates to 1980 so values on
#'  sames days can be compared between years.
#'
#'  @param data Data frame. Column of date data titled "date".
#'  @return Returns original data frame with a new column of normalized dates.
normalize_dates <- function(data=NULL){
  # normalize all dates to 1980 (leap year)
  data$normal.date <- as.Date(paste0("1980", sprintf("%02d", lubridate::month(data$date)),
                                           sprintf("%02d", lubridate::day(data$date))), "%Y%m%d")
  data <- data %>% dplyr::arrange(normal.date)
  data
}

#' Summarize LAI data around a day of the year.
#'
#' \code{bulid_lai_window_summary} summarizes the LAI data across multiple
#'  years in a month wide window around a normalized date.
#'
#'  @param data Data frame. Normalized dates in a column titled "normal date".
#'  @return Returns a data frame summarizing LAI data for available years of
#'    data for each day of the year.
build_lai_window_summary <- function(data=NULL){
  start <- as.Date("1980-01-01", "%Y-%m-%d")
  end <- as.Date("1980-12-31", "%Y-%m-%d")
  window_summary <- data.frame(date = start:end)
  window_summary$date <- as.Date(window_summary$date, origin="1970-01-01")
  for (i in 1:nrow(window_summary)){
    d <- window_summary$date[i]
    l <- dplyr::filter(data, normal.date>(d-14) & normal.date<(d+14))$lai
    window_summary$n[i] <- length(l)
    window_summary$shaptest[i] <- shapiro.test(l)$p.value
    window_summary$running.avg[i] <- mean(l, na.rm=TRUE)
    window_summary$running.sd[i] <- sd(l, na.rm=TRUE)
  }
  window_summary
}

build_lai_window_data <- function(data=NULL){
  start <- as.Date("1980-01-01", "%Y-%m-%d")
  end <- as.Date("1980-12-31", "%Y-%m-%d")
  n.d <- c(start:end)
  n.d <- as.Date(n.d, origin="1970-01-01")
  window_data <- list()
  for (i in 1:length(n.d)){
    d <- n.d[i]
    l <- dplyr::filter(data, normal.date>(d-14) & normal.date<(d+14))$lai
    window_data[[i]] <- l
  }
  names(window_data) <- n.d
  window_data
}



build_auc_stack <- function(lai_stack=NULL, year_index=NULL){
  pb <- txtProgressBar(min=0, max=raster::ncell(lai_stack), width=80, style=3)
  auc_raster <- raster::stack()
  for (k in year_index){
    year_raster <- lai_stack[[1]] * 0
    auc_raster <- raster::stack(auc_raster, year_raster)
  }
  names(auc_raster)[1:raster::nlayers(auc_raster)] <- paste0("y", year_index)
  for (i in 1:raster::ncell(lai_stack)){
    lai_vert <- drill_down(layers=dates_index, stack=lai_stack, point=i)
    colnames(lai_vert) <- c("date", "lai")
    lai_vert <- dplyr::arrange(lai_vert, date)
    lai_vert <- normalize_dates(data=lai_vert)
    for (j in 1:length(year_index)){
      date.limits <- as.Date(c(paste0(year_index[j], "-01-01"), paste0(year_index[j], "-12-31")), "%Y-%m-%d")
      lai_year <- lai_vert %>% filter(date>date.limits[1] & date<date.limits[2])
      bot <- if ((min(lai_year$date) - 8) > date.limits[1]) seq(date.limits[1], min(lai_year$date), 8) else c()
      top <- if ((max(lai_year$date) + 8) < date.limits[2]) seq(max(lai_year$date)+8, date.limits[2], 8) else c()
      patch_dates <- data.frame(date=c(bot, top))
      if (length(patch_dates$date) != 0){
        patch_dates$date <- as.Date(patch_dates$date, origin="1970-01-01")
        patch_dates <- normalize_dates(data=patch_dates)
        patch_dates <- get_patch_data(data=lai_vert, patches=patch_dates)
        patched_year <- dplyr::arrange(rbind(lai_year, patch_dates), date)
      } else{
        patched_year <- dplyr::arrange(lai_year, date)
      }
      if (sum(is.na(patched_year$lai))!=0){
        auc_year <- NA
      } else{
        patched_year$day <- lubridate::yday(patched_year$date)
        a <- loess(lai ~ day, data=patched_year)
        b <- data.frame(x=patched_year$day, y=a$fitted)
        auc_year <- MESS::auc(b$x, b$y, type="spline")
      }
      auc_raster[i][j] <- auc_year
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  auc_raster
}

#' Build a stack of points and one year change in LAI area under curve.
#'
#' \code{build_auc_delta_stack} builds a raster stack of the change in the LAI
#'  area under the curve from the previous year.
#'
#'  @param auc_stack Raster stack. A stack of rasters giving yearly AUC values.
#'    First layer in stack is point.id's, subsequent layers are AUC values.
#'  @param year_index Numeric vector. Vector of years for which AUC data is
#'    contained in auc_stack.
#'  @return Returns raster stack. First layer is point.id's, subsequent layers
#'    are % change in AUC from the previous year.
build_auc_delta_stack <- function(auc_stack=NULL, year_index=NULL){
  delta_raster <- raster::stack()
  for (i in 1:length(year_index)){
    add_raster <- auc_stack[[1]] * 0
    delta_raster <- raster::stack(delta_raster, add_raster)
  }
  names(delta_raster)[1:raster::nlayers(delta_raster)] <- paste0("y", year_index)
  ticks <- raster::ncell(delta_raster) * (raster::nlayers(delta_raster) - 3)
  pb <- txtProgressBar(min=0, max=ticks, style=3, width=80)
  ticker <- 1
  for (j in 1:raster::ncell(delta_raster)){
    print(j)
    auc_vert <- drill_down(layers=paste0("y", year_index), stackname=auc_stack, point=j)
    delta_raster[j][1] <- 0
    for (k in 2:raster::nlayers(delta_raster)){
      delta_raster[j][k] <- (auc_vert$value[k] - auc_vert$value[k - 1]) / auc_vert$value[k - 1]
      setTxtProgressBar(pb, ticker)
      ticker <- ticker + 1
    }
  }
  close(pb)
  delta_raster
}




