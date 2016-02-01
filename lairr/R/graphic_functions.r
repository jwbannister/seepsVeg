#' Plot raster using ggplot.
#'
#' \code{plot_raster} creates a ggplot of a raster.
#'
#'  @param data Raster. A single layer raster to be plotted.
#'  @return Returns a ggplot object.
plot_raster <- function(data=NULL){
  temp <- data.frame(raster::rasterToPoints(data))
  colnames(temp) <- c("x", "y", "value")
  p1 <- ggplot(temp, aes(x=x, y=y)) +
    geom_raster(aes(fill=value)) +
    scale_fill_gradient2(high="#1a9641", midpoint=0, mid="#f7f7f7", low="#d7191c", space="Lab") +
    coord_fixed()
  p1
}

makePNG <- function(plt, filename, ht=6, wt=6, ppi=300){
  png(filename, width=wt*ppi, height=ht*ppi, res=ppi)
  print(plt)
  dev.off()
}

plot_raster_discrete <- function(data=NULL){
  temp <- data.frame(raster::rasterToPoints(data))
  colnames(temp) <- c("x", "y", "value")
  p1 <- ggplot(temp, aes(x=x, y=y)) +
    geom_raster(aes(fill=factor(value))) +
    scale_fill_brewer(breaks=c(0, 1, 2, 3, 4, 5), type="seq", palette="Oranges",
                      limits=c(0, 1, 2, 3, 4, 5), name="Years") +
    coord_fixed()
  p1
}


