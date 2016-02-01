library(dplyr)
library(ggplot2)
library(reshape2)
devtools::load_all("~/Dropbox (airsci)/code/lairr")

# threshold <- "15"
# acre <- read.csv(paste0("./data_raw/acreage/Total_Acre_AUC_gt", threshold, "_3columns.txt"))
# colnames(acre) <- c("dischargezone", "year", "acreage")

threshold <- "45"
acre <- read.csv(paste0("./data_raw/acreage/Total_Acre_AUC_7Zone_gt", threshold, ".txt"))
colnames(acre) <- tolower(colnames(acre))
acre <- melt(acre, id.vars="zonename")
colnames(acre) <- c("dischargezone", "year", "acreage")
acre$year <- as.integer(substring(acre$year, 2))
acre$dischargezone <- factor(acre$dischargezone)

new.acre <- data.frame(dischargezone=factor(), year=as.integer(), acreage=as.numeric())
quants <- data.frame(dischargezone=factor(), min.acreage=as.numeric(), max.acreage=as.numeric(),
                     q25=as.numeric(), q50=as.numeric(),
                     q75=as.numeric())
for (i in levels(acre$dischargezone)[-3]){
  a <- filter(acre, dischargezone==i)
  for (j in 1:nrow(a)){
    kernel_ecdf <- sROC::kCDF(a$acreage[-j], adjust=1)
    val <- a$acreage[j]
    a$quantile[j] <- kernel_ecdf$Fhat[which.min(abs(kernel_ecdf$x - val))]
  }
  new.acre <- rbind(new.acre, a)
  p1 <- ggplot(a, aes(x=acreage)) + geom_density() +
    ggtitle(paste0("Zone ", i, " Acreage PDF - AUC Threshold = ", threshold))
  makePNG(p1, paste0("Zone ", i, " PDF"))
  kern <- sROC::kCDF(a$acreage, adjust=1)
  p2 <- ggplot(a, aes(x=acreage)) + stat_ecdf() +
    geom_point(data=data.frame(x=kern$x, y=kern$Fhat), mapping=aes(x=x, y=y)) +
    ggtitle(paste0("Zone ", i, " Acreage CDF - AUC Threshold = ", threshold))
  makePNG(p2, paste0("Zone ", i, " CDF"))
  b <- data.frame(dischargezone=i, min.acreage=min(a$acreage), max.acreage=max(a$acreage),
                  q25=kern$x[which.min(abs(kern$Fhat - 0.25))],
                  q50=kern$x[which.min(abs(kern$Fhat - 0.5))],
                  q75=kern$x[which.min(abs(kern$Fhat - 0.75))])
  b[b < 0] <- 0
  quants <- rbind(quants, b)
}

write.csv(new.acre, file=paste0("./output/yearly quantile scores (AUC threshold = ", threshold, ").csv"))
write.csv(quants, file=paste0("./output/zone quantile levels (AUC threshold = ", threshold, ").csv"))
