devtools::load_all()
load("../data/owens_index.RData")

load(paste0("../data/", zone_index[1], ".RData"))
auc_df <- raster_to_df(seep_file=zone_index[1], valuestack="auc")
auc_quant_df <- raster_to_df(seep_file=zone_index[1], valuestack="auc_quant")
streak_df <- raster_to_df(seep_file=zone_index[1], valuestack="streak")

for (i in zone_index[2:18]){
  pb <- txtProgressBar(min=0, max=length(zone_index), style=3, width=80)
  load(paste0("../data/", i, ".RData"))
  auc_tmp <- raster_to_df(seep_file=i, valuestack="auc")
  auc_quant_tmp <- raster_to_df(seep_file=i, valuestack="auc_quant")
  streak_tmp <- raster_to_df(seep_file=i, valuestack="streak")
  auc_df <- rbind(auc_df, auc_tmp)
  auc_quant_df <- rbind(auc_quant_df, auc_quant_tmp)
  streak_df <- rbind(streak_df, streak_tmp)
  setTxtProgressBar(pb, i)
}
close(pb)

write.csv(auc_df, "../output/auc.csv")
write.csv(auc_quant_df, "../output/auc_quant.csv")
write.csv(streak_df, "../output/streak.csv")


