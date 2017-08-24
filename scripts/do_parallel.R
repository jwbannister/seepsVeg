load("./data/indices.RData")
load("./data-analysis/lai.RData")
auc_list_mclapply <- parallel::mclapply(split_lai, apply_auc, mc.cores=2)
split_auc <- lapply(auc_list_mclapply, reshape2::melt, id.vars=c("feid"),
                    variable.name="year", value.name="auc")
save(split_auc, file="./data-analysis/auc.RData")
rm(split_lai, auc_list_mclapply)

quant_list_mclapply <- parallel::mclapply(split_auc, apply_quant, mc.cores=2)
split_quant <- lapply(quant_list_mclapply, reshape2::melt, id.vars=c("feid"),
                      variable.name="year", value.name="quant")
save(split_quant, file="./data-analysis/quant.RData")
rm(split_auc, quant_list_mclapply)

streak_list_mclapply <- parallel::mclapply(split_quant, apply_score,
                                           threshold=0.1, mc.cores=2)
split_streak <- lapply(streak_list_mclapply, reshape2::melt, id.vars=c("feid"),
                       variable.name="year", value.name="streak")
save(split_streak, file="./data-analysis/streak.RData")
