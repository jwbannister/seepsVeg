library(magrittr)
library(ggplot2)
devtools::load_all()

load("../data/owens_index.RData")

load(paste0("../data/additional_area.RData"))

for (j in zone_index[1:17]){
  load(paste0("../data/", j, ".RData"))
  auc_max <- max(auc[ , , ], na.rm=TRUE)
  auc_min <- min(auc[ , , ], na.rm=TRUE)
  dir.create(paste0("../output/", j))
  dir.create(paste0("../output/", j, "/auc"))
  dir.create(paste0("../output/", j, "/quant"))
  for (i in year_index){
    yr <- i
    ind <- which(year_index==yr)
    p1 <- isolate_area(zonelayer=points$"zone", valuelayer=auc[[ind]]) %>%
      plot_raster() + ggtitle(paste0("AUC - ", j, ", ", as.character(yr))) +
      scale_fill_gradient2(high="#1a9641", low="#f7f7f7", space="Lab",
                           breaks=seq(100,auc_max, 100), limits=c(0, auc_max))
    makePNG(p1, paste0("../output/", j, "/auc/auc_", yr, ".png"))

    p2 <- isolate_area(zonelayer=points$"zone", valuelayer=auc_quant[[ind]]) %>%
      plot_raster() + ggtitle(paste0("AUC Quantile Scores - ", j, ", ", as.character(yr))) +
      scale_fill_gradient2(low="#d7191c", mid="#f7f7f7", high="#1a9641", midpoint=0.5, space="Lab",
                           breaks=c(0.25, 0.5, 0.75), limits=c(0, 1))
    makePNG(p2, paste0("../output/", j, "/quant/quant_", yr, ".png"))
  }
}

for (j in zone_index[1:17]){
  load(paste0("../data/", j, ".RData"))
  dir.create(paste0("../output/", j, "/occur"))
  for (i in 1:length(year_index)){
    yr <- year_index[i]
    p3 <- build_quantile_df(valuelayer=auc_quant[[i]]) %>%
      ggplot(aes(x=Var1, y=percent)) +
      geom_point() + xlab("Expected") + ylab("Observed") +
      ggtitle(paste0("Expected vs. Observed Quantile Occurance - ", j, ", ", as.character(yr))) +
      geom_abline(intercept=0, slope=1, color="red")
    makePNG(p3, paste0("../output/", j, "/occur/occur_", yr, ".png"))
  }
}

for (j in zone_index){
  load(paste0("../data/", j, ".RData"))
  dir.create(paste0("../output/", j, "/streak"))
  for (i in 2:length(year_index)){
    yr <- year_index[i]
    p4 <- isolate_area(zonelayer=points$"zone", valuelayer=streak[[i]]) %>%
      plot_raster_discrete() +
      ggtitle(paste0("<0.1 Quantile Streak - ", j, ", ", as.character(yr)))
    makePNG(p4, paste0("../output/", j, "/streak/streak_", yr, ".png"))
  }
}

isolate_area(zonelayer=points$"zone", valuelayer=streak[[14]]) %>%
  plot_raster_discrete()


auc_threshold_stack <- build_threshold_stack(quant_stack=auc_quant,
                                             lower=low.thresh, upper=high.thresh)
auc_metrics <- calc_metrics(data=auc_quant, lower_threshold=low.thresh,
                            upper_threshold=high.thresh)
low.thresh <- 0.25
high.thresh <- 0.75

years.trend <- 5
trend_raster <- build_trend_raster(threshold_stack=auc_threshold_stack,
                                   years_back=years.trend)
isolate_area(zonelayer=points$"zone", valuelayer=trend_raster) %>%
  plot_raster() + ggtitle(paste0(as.character(years.trend), "-year Threshold Trend - ", seep_title, ", ", as.character(yr)))



moran.max <- 0
moran.min <- 0
pb <- txtProgressBar(min=0, max=length(year_index), style=3, width=80)
for (i in 1:length(year_index)){
  a <- isolate_area(zonelayer=points$"zone", valuelayer=auc_quant[[i]]) %>%
    raster::MoranLocal()
  f <- max(a[ , ][!is.na(a[ , ])])
  g <- min(a[ , ][!is.na(a[ , ])])
  moran.max <- max(f, moran.max)
  moran.min <- min(g, moran.min)
  setTxtProgressBar(pb, i)
}
close(pb)

isolate_area(zonelayer=points$"zone", valuelayer=auc_quant[[ind]]) %>%
  raster::MoranLocal()

dplyr::select(auc_metrics, year, mid.metric, moran.i) %>%
  reshape2::melt(id.vars="year") %>%
ggplot(aes(x=year, y=value)) +
  geom_point() +
  geom_path(color="red") +
  facet_grid(variable ~ ., scales="free_y")

isolate_area(zonelayer=points$"zone", valuelayer=auc_quant[[ind]]) %>%
  raster::MoranLocal() %>%
  plot_raster() + ggtitle(paste0("AUC Local Moran Scores - ", seep_title, ", ", as.character(yr))) +
  scale_fill_gradient2(high="#1a9641", midpoint=0, mid="#f7f7f7", low="#d7191c", space="Lab",
                       limits=c(moran.min, moran.max))
