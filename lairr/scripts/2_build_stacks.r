devtools::load_all()
load("../data/owens_lai.RData")
load("../data/owens_index.RData")

for (i in levels(owens_lai$zone)[3:18]){
  points <- build_points_stack(area=i)
  lai <- build_lai_stack(area=i)
  auc <- build_auc_stack_2(lai_stack=lai, year_index=year_index)
  auc[auc==0] <- 0.00001
  auc_quant <- build_xval_quantile_stack(value_stack=auc, year_index=year_index)
  save(points, lai, auc, auc_quant, file=paste0("../data/", i, ".RData"))
}

for (j in zone_index){
  print(j)
  load(paste0("../data/", j, ".RData"))
  streak <- auc
  pb <- txtProgressBar(min=0, max=raster::ncell(streak), width=80, style=3)
  for (i in 1:raster::ncell(streak)){
    streak[i][1] <- NA
    for (k in 2:length(year_index)){
      n <- 0
      for (m in 0:(k-1)){
        if (is.na(auc_quant[i][k-m])){
          break
        }
        if (auc_quant[i][k-m] < 0.1){
          n <- n + 1
        } else {
          break
        }
      }
      streak[i][k] <- n
    }
    setTxtProgressBar(pb, i)
  }
  save(points, lai, auc, auc_quant, streak, file=paste0("../data/", j, ".RData"))
}
close(pb)
