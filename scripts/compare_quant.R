library(ggplot2)
library(dplyr)

load("./data-analysis/quant.RData")
xval_quant <- split_quant
rm(split_quant)
xval_quant <- Reduce(rbind, xval_quant)

quant <- load("./data-analysis/quant_noxval.RData")
quant <- split_quant
rm(split_quant)
quant <- Reduce(rbind, quant)

join_quant <- inner_join(quant, xval_quant, by=c("feid", "year"))

ggplot(join_quant, aes(x=quant.x, y=quant.y)) +
  geom_point()
