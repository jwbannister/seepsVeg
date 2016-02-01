devtools::load_all("~/Dropbox (airsci)/code/lairr")

library(ggplot2)
library(sROC)
auc_vals <- c(130, 140, 150, 122, 80, 117, 194, 173, 163, 102,
              12, 155, 174, 133, 77, 201, 15, 184, 144, 155,
              223, 164, 117, 201, 199, 8, 163, 179, 180, 200)
a <- data.frame(year=c(1984:2013), auc=auc_vals)

p1 <- ggplot(a, aes(x=auc)) +
  ggtitle("Probability Density Plot") +
  geom_density() +
  geom_vline(xintercept=80, color="red")
makePNG(p1, "pdf", ht=4)

kern <- sROC::kCDF(a$auc, adjust=0.5)

p2 <- ggplot(a, aes(x=auc))+
  ggtitle("Cumulative Density Plot") +
  stat_ecdf() +
  geom_point(data=data.frame(x=kern$x, Fhat=kern$Fhat), mapping=aes(x=x, y=Fhat)) +
  geom_vline(xintercept=80, color="red")
makePNG(p2, "cdf", ht=4)
