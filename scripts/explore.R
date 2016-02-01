library(ggplot2)
library(dplyr)
library(RColorBrewer)

load("./data-analysis/pixels.RData")
load("./data-analysis/auc.RData")
load("./data-analysis/quant.RData")
load("./data-analysis/streak.RData")

auc_df <- Reduce(rbind, split_auc)
quant_df <- Reduce(rbind, split_quant)
streak_df <- Reduce(rbind, split_streak)
streak_df$streak <- factor(streak_df$streak)
levels(streak_df$streak) <- c("0", "1", "2", "3", "4", "5+")

rm(split_auc, split_quant, split_streak)

filter(auc_df, auc <= 0)

max_auc <- group_by(auc_df, year) %>% summarize(max(auc, na.rm=TRUE))
lim <- min(max_auc[ , 2])
for (i in year_index){
  plt <- auc_df %>% filter(year==i) %>%
    inner_join(pixels_df, by="feid") %>%
    ggplot(aes(x=x, y=y)) +
    geom_tile(aes(fill=auc)) +
    coord_fixed() +
    scale_fill_gradientn(limits=c(0, lim), colours=brewer.pal(9, "YlGn")) +
    ggtitle(paste0("AUC -", i))
  makePNG(plt, paste0("./output/auc/y", i, ".png"))
}

for (i in year_index){
  plt <- quant_df %>% filter(year==i) %>%
    inner_join(pixels_df, by="feid") %>%
    ggplot(aes(x=x, y=y)) +
    geom_tile(aes(fill=quant)) +
    coord_fixed() +
    scale_fill_gradient2(limits=c(0, 1), low="red", mid="white", high="green",
                         midpoint=0.5, space="Lab")+
    ggtitle(paste0("quant -", i))
  makePNG(plt, paste0("./output/quant/y", i, ".png"))
}

for (i in year_index){
  plt <- streak_df %>% filter(year==i) %>%
    inner_join(pixels_df, by="feid") %>%
    ggplot(aes(x=x, y=y)) +
    geom_tile(aes(fill=streak)) +
    coord_fixed() +
    scale_fill_brewer(limits=c("0", "1", "2", "3", "4", "5+"), type="seq",
                      palette="Oranges", name="Years") +
    ggtitle(paste0("streak -", i))
  makePNG(plt, paste0("./output/streak/y", i, ".png"))
}
