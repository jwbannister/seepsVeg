build_xval_quantile_stack <- function(value_stack=NULL, year_index=NULL){
  ticker <- 1
  ticks <- raster::ncell(value_stack) * (raster::nlayers(value_stack) - 1)
  pb <- txtProgressBar(min=0, max=ticks, style=3, width=80)
  quant_raster <- raster::stack()
  for (i in year_index){
    year_raster <- value_stack[[1]] * 0
    quant_raster <- raster::stack(quant_raster, year_raster)
  }
  names(quant_raster)[1:raster::nlayers(quant_raster)] <- paste0("y", year_index)
  for (p in 1:raster::ncell(quant_raster)){
    print(p)
    vert <- drill_down(layers=paste0("y", year_index), stackname=value_stack, point=p)
    for (j in 1:nrow(vert)){
      if (is.na(sum(vert$value)) | sum(vert$value) < 0.001){
        vert$quantile[j] <- NA
      } else{
      kernel_ecdf <- sROC::kCDF(vert$value[-j], adjust=.5)
      val <- vert$value[j]
      vert$quantile[j] <- kernel_ecdf$Fhat[which.min(abs(kernel_ecdf$x - val))]
      }
    }
    for (k in 1:raster::nlayers(quant_raster)){
      quant_raster[p][k] <- vert$quantile[k]
      setTxtProgressBar(pb, ticker)
      ticker <- ticker + 1
    }
  }
  close(pb)
  quant_raster
}
