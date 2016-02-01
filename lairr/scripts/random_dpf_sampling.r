devtools::load_all("~/Dropbox (airsci)/code/lairr")
library(ggplot2)
library(dplyr)
library(raster)
library(gridExtra)

makePNG <- function(plt, filename, ht=6, wt=6, ppi=300){
  png(paste0(filename), width=wt*ppi, height=ht*ppi, res=ppi)
  print(plt)
  dev.off()
}

# mv_dpf <- read.csv("~/Dropbox (airsci)/projects/owenslake/managed_veg/data/DPF_grid.csv")
# colnames(mv_dpf) <- tolower(colnames(mv_dpf))

for (i in c("ash_creek", "swedes_pasture", "cottonwood_marsh",
            "northwest_spring", "crystal_geyser")){
  load(paste0("./data/", i, ".RData"))
  temp <- stack(points$"zone", lai[[1]] * 0)
  names(temp) <- c("zone", "max.lai_2014")
  pb <- txtProgressBar(min=0, max=ncell(temp), style=3, width=80)
  for (j in 1:ncell(temp)){
    temp[[2]][j] <- max(lai[[700:710]][j], na.rm=TRUE)
    setTxtProgressBar(pb, j)
  }
  close(pb)
  temp$"max.lai_2014" <- (temp$"max.lai_2014"[ , ] != -Inf) * temp$"max.lai_2014"
  temp$"max.lai_2014" <- isolate_area(zonelayer=temp$"zone", valuelayer=temp$"max.lai_2014")
  temp <- stack(temp, GearyLocal(temp$"max.lai_2014"))
  names(temp)[3] <- "local.geary"
  set.seed(42)
  samp <- sample(Which(temp$"max.lai_2014" > 0 & !is.na(temp$"max.lai_2014"), cell=TRUE), 30)
  assign(i, temp)
  assign(paste0(i, ".sample"), data.frame(cell=samp, feid=points$"point.id"[samp], xyFromCell(temp, samp), max.lai_2014=temp$"max.lai_2014"[samp], local.geary=temp$"local.geary"[samp]))
}
rm(lai, auc, temp, auc_delta, auc_quant, auc_delta_quant, points, i, j, pb, samp)

p1 <- ggplot(ash_creek.sample, aes(x=max.lai_2014)) +
  geom_density(aes(color="Sample Distribution")) +
  geom_density(data=data.frame(max.lai_2014=ash_creek$"max.lai_2014"[ , ]), 
               mapping=aes(x=max.lai_2014, color="Population Distribution")) +
  theme(legend.title=element_blank(), legend.position=c(.75, .75)) +
  ggtitle("Ash Creek")
p2 <- plot_raster(ash_creek$"max.lai_2014") +
  geom_point(mapping=aes(x=x, y=y, color="Sample Point"), data=ash_creek.sample)  +
  theme(legend.title=element_blank()) +
  ggtitle("Ash Creek")
makePNG(grid.arrange(p1, p2, ncol=2), "./output/dpf_sampling/ash_creek.png", ht=5, wt=10)
write.csv(ash_creek.sample, file="./output/dpf_sampling/ash_creek.csv", row.names=FALSE)

p1 <- ggplot(swedes_pasture.sample, aes(x=max.lai_2014)) +
  geom_density(aes(color="Sample Distribution")) +
  geom_density(data=data.frame(max.lai_2014=swedes_pasture$"max.lai_2014"[ , ]), 
               mapping=aes(x=max.lai_2014, color="Population Distribution")) +
  theme(legend.title=element_blank(), legend.position=c(.75, .75)) +
  ggtitle("Swedes Pasture")
p2 <- plot_raster(swedes_pasture$"max.lai_2014") +
  geom_point(mapping=aes(x=x, y=y, color="Sample Point"), data=swedes_pasture.sample)  +
  theme(legend.title=element_blank()) +
  ggtitle("Swedes Pasture")
makePNG(grid.arrange(p1, p2, ncol=2), "./output/dpf_sampling/swedes_pasture.png", ht=5, wt=10)
write.csv(swedes_pasture.sample, file="./output/dpf_sampling/swedes_pasture.csv", row.names=FALSE)

p1 <- ggplot(cottonwood_marsh.sample, aes(x=max.lai_2014)) +
  geom_density(aes(color="Sample Distribution")) +
  geom_density(data=data.frame(max.lai_2014=cottonwood_marsh$"max.lai_2014"[ , ]), 
               mapping=aes(x=max.lai_2014, color="Population Distribution")) +
  theme(legend.title=element_blank(), legend.position=c(.75, .75)) +
  ggtitle("Cottonwood Marsh")
p2 <- plot_raster(cottonwood_marsh$"max.lai_2014") +
  geom_point(mapping=aes(x=x, y=y, color="Sample Point"), data=cottonwood_marsh.sample)  +
  theme(legend.title=element_blank()) +
  ggtitle("Cottonwood Marsh")
makePNG(grid.arrange(p1, p2, ncol=2), "./output/dpf_sampling/cottonwood_marsh.png", ht=5, wt=10)
write.csv(cottonwood_marsh.sample, file="./output/dpf_sampling/cottonwood_marsh.csv", row.names=FALSE)

p1 <- ggplot(northwest_spring.sample, aes(x=max.lai_2014)) +
  geom_density(aes(color="Sample Distribution")) +
  geom_density(data=data.frame(max.lai_2014=northwest_spring$"max.lai_2014"[ , ]), 
               mapping=aes(x=max.lai_2014, color="Population Distribution")) +
  theme(legend.title=element_blank(), legend.position=c(.75, .75)) +
  ggtitle("Northwest Spring")
p2 <- plot_raster(northwest_spring$"max.lai_2014") +
  geom_point(mapping=aes(x=x, y=y, color="Sample Point"), data=northwest_spring.sample)  +
  theme(legend.title=element_blank()) +
  ggtitle("Northwest Spring")
makePNG(grid.arrange(p1, p2, ncol=2), "./output/dpf_sampling/northwest_spring.png", ht=5, wt=10)
write.csv(northwest_spring.sample, file="./output/dpf_sampling/northwest_spring.csv", row.names=FALSE)

p1 <- ggplot(crystal_geyser.sample, aes(x=max.lai_2014)) +
  geom_density(aes(color="Sample Distribution")) +
  geom_density(data=data.frame(max.lai_2014=crystal_geyser$"max.lai_2014"[ , ]), 
               mapping=aes(x=max.lai_2014, color="Population Distribution")) +
  theme(legend.title=element_blank(), legend.position=c(.75, .75)) +
  ggtitle("Crystal Geyser")
p2 <- plot_raster(crystal_geyser$"max.lai_2014") +
  geom_point(mapping=aes(x=x, y=y, color="Sample Point"), data=crystal_geyser.sample)  +
  theme(legend.title=element_blank()) +
  ggtitle("Crystal Geyser")
makePNG(grid.arrange(p1, p2, ncol=2), "./output/dpf_sampling/crystal_geyser.png", ht=5, wt=10)
write.csv(crystal_geyser.sample, file="./output/dpf_sampling/crystal_geyser.csv", row.names=FALSE)
