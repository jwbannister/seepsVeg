load("./data/indices.RData")
load("./data-analysis/lai.RData")
auc_list_mclapply <- parallel::mclapply(split_lai, apply_auc, mc.cores=36)
split_auc <- lapply(auc_list_mclapply, reshape2::melt, id.vars=c("feid"),
                    variable.name="year", value.name="auc")
save(split_auc, file="./data-analysis/auc.RData")

quant_list_mclapply <- parallel::mclapply(split_auc, apply_quant, mc.cores=36)
split_quant <- lapply(quant_list_mclapply, reshape2::melt, id.vars=c("feid"),
                      variable.name="year", value.name="quant")
save(split_quant, file="./data-analysis/quant.RData")

streak_list_mclapply_0.1 <- parallel::mclapply(split_quant, apply_score,
                                           threshold=0.1, mc.cores=36)
split_streak_0.1 <- lapply(streak_list_mclapply, reshape2::melt, id.vars=c("feid"),
                       variable.name="year", value.name="streak")
streak_list_mclapply_0.2 <- parallel::mclapply(split_quant, apply_score,
                                           threshold=0.2, mc.cores=36)
split_streak_0.2 <- lapply(streak_list_mclapply, reshape2::melt, id.vars=c("feid"),
                       variable.name="year", value.name="streak")
streak_list_mclapply_0.3 <- parallel::mclapply(split_quant, apply_score,
                                           threshold=0.3, mc.cores=36)
split_streak_0.3 <- lapply(streak_list_mclapply, reshape2::melt, id.vars=c("feid"),
                       variable.name="year", value.name="streak")
save(split_streak_0.1, split_streak_0.2, split_streak_0.3, 
     file="./data-analysis/streak.RData")
