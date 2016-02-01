library(raster)
library(ggplot2)
library(dplyr)
library(gridExtra)

makePNG <- function(plt, filename, ht=6, wt=6, ppi=300){
  png(paste0(filename), width=wt*ppi, height=ht*ppi, res=ppi)
  print(plt)
  dev.off()
}

transects <- rgdal::readOGR("../data_raw/UAV_Transects_rev2", "UAV_Transects_draft")

zone_index <- c("Swedes Pasture", "Ash Creek", "Northwest Spring",
                "Cottonwood Marsh", "Whiskey Springs")

zone.transects <- data.frame(x=numeric(), y=numeric(), max.lai=numeric(), zone=character())
zone.samples <- data.frame(x=numeric(), y=numeric(), max.lai=numeric(), zone=character())
for (i in zone_index){
  seep_file <- tolower(gsub(" ", "_", i))
  load(paste0("~/dropbox/projects/owenslake/seeps_springs/analysis/data/", seep_file, ".RData"))
  max.raster <- max(lai[[700:710]], na.rm=TRUE)
  trans.raster <- raster::rasterize(transects, max.raster)
  crs(max.raster)@projargs <- crs(trans.raster)@projargs
  pb <- txtProgressBar(min=0, max=ncell(max.raster), style=3, width=80)
  for (j in 1:ncell(max.raster)){
    max.raster[j] <- if (points$"zone"[j]>0) max.raster[j] else NA
    setTxtProgressBar(pb, j)
  }
  close(pb)
  temp <- raster::stack(max.raster, trans.raster,
                        overlay(trans.raster, max.raster,
                                fun=function(x, y){return(x*y/x)}))
  transects_df <- data.frame(x=coordinates(temp$"layer.3")[ , 1],
                     y=coordinates(temp$"layer.3")[ , 2],
                     max.lai=temp$"layer.3"[ , ], zone=i)
  temp2 <- transects_df[!is.na(transects_df$max.lai), ]
  set.seed(1)
  samp <- temp2[sample(nrow(temp2), 30), ]
  zone.transects <- rbind(zone.transects, transects_df)
  zone.samples <- rbind(zone.samples, samp)
  assign(paste0(seep_file, "_df"), data.frame(x=coordinates(max.raster)[ , 1],
                                              y=coordinates(max.raster)[ , 2],
                                              max.lai=max.raster[ , ]))
}

rm(auc, auc_delta, auc_quant, auc_delta_quant, lai, points, i, seep_file,
   trans.raster, temp, temp2, samp, transects_df)

write.csv(zone.samples, file="../output/dpf_sampling/DPF_sample_locations.csv", row.names=FALSE)

for (i in zone_index){
  zn <- i
  seep_file <- tolower(gsub(" ", "_", i))
  whole_zone <- get(paste0(seep_file, "_df"))

  p0 <- ggplot(whole_zone, aes(x=x, y=y)) +
    geom_tile(aes(fill=max.lai)) +
    scale_fill_gradient2(high="#1a9641", midpoint=0, mid="#f7f7f7", low="#d7191c", space="Lab", guide="none") +
    geom_point(data=filter(zone.samples, zone==zn), mapping=aes(x=x, y=y, color="DPF Location")) +
    theme(legend.title=element_blank()) +
    coord_fixed()
  
  p1 <- ggplot(filter(zone.transects, zone==zn), aes(x=max.lai)) +
    geom_density(aes(color="Transects")) +
    geom_density(data=filter(zone.samples, zone==zn), mapping=aes(x=max.lai, color="Sample")) +
    geom_density(data=whole_zone, aes(x=max.lai, color="Zone")) +
    theme(legend.title=element_blank()) +
    ggtitle(zn)


  makePNG(grid.arrange(p1, p0, ncol=2), paste0("../output/dpf_sampling/", seep_file, ".png"), ht=5, wt=10)
}

