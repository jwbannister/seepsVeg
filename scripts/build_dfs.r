devtools::load_all()
library(reshape2)

load("./data-analysis/auc.RData")
auc_df <- Reduce(rbind, split_auc)
auc_df$year <- paste0("y", auc_df$year)
auc_wide <- dcast(auc_df, feid ~ year)
write.csv(auc_wide, file="./output/auc.csv", row.names=FALSE)

load("./data-analysis/quant.RData")
quant_df <- Reduce(rbind, split_quant)
quant_df$year <- paste0("y", quant_df$year)
quant_wide <- dcast(quant_df, feid ~ year)
write.csv(quant_wide, file="./output/quant.csv", row.names=FALSE)

load("./data-analysis/streak.RData")
streak_df <- Reduce(rbind, split_streak)
streak_df$year <- paste0("y", streak_df$year)
streak_wide <- dcast(streak_df, feid ~ year)
write.csv(streak_wide, file="./output/streak.csv", row.names=FALSE)
