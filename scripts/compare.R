library(reshape2)
library(dplyr)
library(ggplot2)
load("./data-analysis/pixels.RData")
load("./data-analysis/auc.RData")
auc_df <- Reduce(rbind, split_auc)
rm(split_auc)

lairr_auc <- read.csv("../lairr/output/auc.csv") %>%
  select(-X) %>% melt(id.vars="FEID") %>%
  mutate(year=substring(variable, 2), auc=value) %>%
  select(-variable, -value)
colnames(lairr_auc) <- tolower(colnames(lairr_auc))
join_auc <- inner_join(lairr_auc, auc_df, by=c("feid", "year"))
join_auc$diff <- join_auc$auc.x - join_auc$auc.y
join_auc$off <- ifelse(join_auc$diff > (0.1 * join_auc$auc.x), TRUE, FALSE)
join_auc <- inner_join(join_auc, pixels_df, by="feid")
tbl <- dplyr::summarize(group_by(join_auc, year), val=sum(off, na.rm=TRUE))

ggplot(join_auc, aes(x=auc.x, y=auc.y)) +
  geom_point()

ggplot(filter(join_auc, year==2012), aes(x=x, y=y)) +
  geom_tile(aes(fill=off)) +
  coord_fixed()
worst <- arrange(filter(join_auc, year==2012, off==TRUE), desc(diff))
best <- arrange(filter(join_auc, year==2012, off==FALSE, auc.x>100), diff)

load("./data-analysis/lai.RData")
lai_2012 <- lapply(split_lai, dplyr::filter, year==2012)
lai2012_df <- Reduce(rbind, lai_2012)
lai2012_df <- inner_join(lai2012_df, pixels_df, by="feid")
day_index <- unique(lai2012_df$day)
ggplot(filter(lai2012_df, day==day_index[10]), aes(x=x, y=y)) +
  geom_tile(aes(fill=lai)) +
  coord_fixed()

pt <- 3
ggplot(filter(lai2012_df, feid==worst$feid[pt]), aes(x=day, y=lai)) +
  geom_point() +
  ggtitle(paste0(worst$feid[pt], "- diff = ", worst$diff[pt])) +
  geom_smooth()

pt <- 5
ggplot(filter(lai2012_df, feid==best$feid[pt]), aes(x=day, y=lai)) +
  geom_point() +
  ggtitle(paste0(best$feid[pt], "- diff = ", best$diff[pt])) +
  geom_smooth()
