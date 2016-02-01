devtools::load_all()
load("./data-analysis/lai.RData")

auc_list <- build_auc_list(split_lai)
split_auc <- lapply(auc_list, reshape2::melt, id.vars=c("feid"),
                    variable.name="year", value.name="auc")
save(split_auc, file="./data-analysis/auc.RData")
rm(split_lai, auc_list)

quant_list <- xval_quantile(split_auc)
split_quant <- lapply(quant_list, reshape2::melt, id.vars=c("feid"),
                    variable.name="year", value.name="quant")
save(split_quant, file="./data-analysis/quant.RData")
rm(split_auc, quant_list)

streak_list <- score_low_streak(split_quant, 0.1)
split_streak <- lapply(streak_list, reshape2::melt, id.vars=c("feid"),
                    variable.name="year", value.name="streak")
save(split_streak, file="./data-analysis/streak.RData")




