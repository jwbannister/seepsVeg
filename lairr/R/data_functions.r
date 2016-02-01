#' Build a point grid.
#'
#' \code{build_area_p;oint_grid} builds a full rectangular point grid for an
#'  area.
#'
#'  @param area Character string.
#'  @param data Data frame. Should include all applicable areas in a "zone"
#'    column, along with "x" and "y" coordinates for each point in the zone.
#'  @return Returns data frame with full rectangle of points encompassing area.
#'    Points in grid that are included in LAI measurements are assigned their
#'    respective point.id. Points not included in LAI have point.id = NA.
build_area_point_grid <- function(area=NULL, data=NULL){
  points <- dplyr::filter(data, zone==area)
  grid <- expand.grid(x=unique(points$x), y=unique(points$y))
  point_grid <- dplyr::left_join(grid, data, by=c("x", "y"))
  point_grid$zone <- as.numeric(factor(point_grid$zone))
  point_grid$zone[is.na(point_grid$zone)] <- 0
  point_grid
}

build_point_grid <- function(data=NULL){
  grid <- expand.grid(x=unique(data$x), y=unique(data$y))
  point_grid <- dplyr::left_join(grid, data, by=c("x", "y"))
  # point_grid$zone <- as.numeric(factor(point_grid$zone))
  # point_grid$zone[is.na(point_grid$zone)] <- 0
  point_grid
}

#' Create raster.
#'
#' \code{create_raster} builds a rater object out of a 3 column data frame.
#'
#'  @param data Data frame. A 3 column data frame with first two columns giving
#'    point coordinates and labeled "x" and "y". Third column will be raster
#'    point values.
#'  @return Returns raster of data with extents to encompass all points in
#'    data frame.
create_raster <- function(data=NULL){
  sp::coordinates(data) <- ~ x + y
  sp::gridded(data) <- TRUE
  raster_df <- raster::raster(data)
  raster_df
}


#' Get values from all layers of raster stack.
#'
#' \code{drill_down} retrieves the value for a given point in all layers of a
#'  raster stack. The raster stack first layer should be point IDs, all
#'  subsequent layers should be values.
#'
#'  @param layers Vector. A vector to label the layers retrieved from the stack.
#'  @param stack RasterStack.
#'  @param point Integer. The point for which to retrieve values.
#'  @return Returns a data frame.
drill_down <- function(layers=NULL, stackname=NULL, point=NULL){
  vert <- data.frame(layer=layers, value=stackname[point][1:raster::nlayers(stackname)])
  vert
}

#' Strip a raster down to pre-defined zone.
#'
#' \code{isolate_area} compares a raster of values to a raster giving zone
#'  classification values and assigns a zero value to all points not contained
#'  in the main zone classification.
#'
#'  @param zonelayer Raster. A raster with cells assigned a numer corresponding
#'    to their seeps and springs zone, and NA if outside of an assigned zone.
#'  @param valuelayer Raster. A raster with values assigned to cells.
#'  @return Returns a raster with all points within the zone still holding
#'    their original value, and all points outside the zone assigned a value
#'    of NA.
isolate_area <- function(zonelayer=NULL, valuelayer=NULL){
  tmp <- sort(table(zonelayer[ , ]), decreasing=TRUE)
  tmp <- tmp[names(tmp)!="0"]
  num <- as.numeric(names(tmp)[1])
  isolate_raster <- zonelayer
  for (i in 1:raster::ncell(isolate_raster)){
    isolate_raster[i] <- if (isolate_raster[i]==num) 1 else NA
  }
  area_only_raster <- valuelayer * isolate_raster
  area_only_raster
}

build_quantile_df <- function(zonelayer=points$"zone", valuelayer=NULL){
  a <- isolate_area(zonelayer=zonelayer, valuelayer=valuelayer)
  b <- table(a[ , ])
  df <- data.frame(b)
  df$Var1 <- as.numeric(as.character(df$Var1))
  for (i in 1:nrow(df)){
    df$percent[i] <- sum(df$Freq[1:i]) / sum(df$Freq)
  }
  df
}

build_simulated_df <- function(df=NULL){
  set.seed(2)
  random_df <- data.frame(Var1=msm::rtnorm(nrow(df), lower=0, upper=1, mean=0.5, sd=0.341),
                          Freq=rep(1, nrow(df)))
  random_df <- dplyr::arrange(random_df, Var1)
  for (i in 1:nrow(random_df)){
    random_df$percent[i] <- sum(random_df$Freq[1:i]) / sum(random_df$Freq)
  }
  random_df
}

calc_metrics <- function(data=NULL, lower_threshold=NULL, upper_threshold=NULL){
  pb <- txtProgressBar(min=0, max=length(year_index), style=3, width=80)
  veg_metrics <- data.frame(year=year_index, low.metric=rep(NA, length(year_index)),
                            high.metric=rep(NA, length(year_index)),
                            mid.metric=rep(NA, length(year_index)),
                            mid.quant=rep(NA, length(year_index)))
  for (i in 1:length(year_index)){
    df <- build_quantile_df(zonelayer=points$"zone", valuelayer=data[[i]])
    lowdiff <- abs(lower_threshold - df$Var1)
    veg_metrics$low.metric[i] <- df$percent[which(lowdiff==min(abs(lower_threshold - df$Var1)))] - lower_threshold
    highdiff <- abs(upper_threshold - df$Var1)
    veg_metrics$high.metric[i] <- (1 - df$percent[which(highdiff==min(abs(upper_threshold - df$Var1)))]) -
      (1 - upper_threshold)
    middiff <- abs(0.5 - df$Var1)
    veg_metrics$mid.metric[i] <- df$percent[which(middiff==min(abs(0.5 - df$Var1)))] - 0.5
    veg_metrics$moran.i[i] <- raster::Moran(data[[i]])
    setTxtProgressBar(pb, i)
  }
  for(j in 1:nrow(veg_metrics)){
    adj <- (max(veg_metrics$mid.metric[-j]) - min(veg_metrics$mid.metric[-j])) / 100
    kernel_ecdf <- sROC::kCDF(veg_metrics$mid.metric[-j], adjust=adj)
    val <- veg_metrics$mid.metric[j]
    veg_metrics$mid.quant[j] <- kernel_ecdf$Fhat[which.min(abs(kernel_ecdf$x - val))]
  }
  close(pb)
  veg_metrics
}

build_threshold_stack <- function(quant_stack=NULL, lower=NULL, upper=NULL){
  pb <- txtProgressBar(min=0, max=length(year_index), style=3, width=80)
  logical_stack <- raster::stack()
  for (i in 1:length(year_index)){
    b <- quant_stack[[i]]
    for (j in 1:raster::ncell(b)){
      if (b[j] >= upper){
        b[j] <- 1
      }
      if (b[j] <= lower){
        b[j] <- -1
      }
      if (b[j] > lower & b[j] < upper){
        b[j] <- 0
      }
    }
    logical_stack <- raster::stack(logical_stack, b)
    setTxtProgressBar(pb, i)
  }
  close(pb)
  logical_stack
}

build_trend_raster <- function(threshold_stack=NULL, years_back=NULL){
  if (ind - years_back < 1) stop("Data doesn't go back far enough!")
  low <- threshold_stack[[ind]]
  low[low==1] <- 0
  low <- abs(low)
  high <- threshold_stack[[ind]]
  high[high==-1] <- 0
  for (i in 0:(years_back-1)){
    low_hist <- threshold_stack[[ind-i]]
    low_hist[low_hist==1] <- 0
    low_hist <- abs(low_hist)
    high_hist <- threshold_stack[[ind-i]]
    high_hist[high_hist==-1] <- 0
    low <- low * low_hist
    high <- high * high_hist
  }
  low <- low * -1
  trend_raster <- low + high
}


#' Convert a raster to a data frame.
#'
#' \code{raster_to_df} converts a raster stack of yearly values into a data
#'  in the format for sending to Chong (Formation).
#'
#'  @param seep_file Character string. The name of the discharge zone,
#'    lower case and _ for spaces.
#'  @param valuestack Character string. The stack object to data frame - ize.
#'  @return Writes the data frame to a .CSV file in the "output" subdirectory
#'    of the working directory.
raster_to_df <- function(seep_file=NULL, valuestack=NULL){
  values <- get(valuestack)
  df <- data.frame(FEID=points$"feid"[ , ])
  for (i in 1:raster::nlayers(values)){
    a <- data.frame(values[[i]][ , ])
    df <- cbind(df, a)
  }
  colnames(df)[2:ncol(df)] <- names(values)
  df
}
