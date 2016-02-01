
# find point high, low and middle auc and check kernel

vert <- drill_down(layers=paste0("y", year_index), stackname=auc, point=900)

j <- 2
year_index[j]

kernel_ecdf <- sROC::kCDF(vert$value[-j], adjust=.5)
val <- vert$value[j]
vert$quantile[j] <- kernel_ecdf$Fhat[which.min(abs(kernel_ecdf$x - val))]

ggplot(data.frame(x=kernel_ecdf$x, Fhat=kernel_ecdf$Fhat), aes(x=x, y=Fhat)) +
  geom_point() +
  stat_ecdf(mapping=aes(x=value, y=NULL), data=vert)
