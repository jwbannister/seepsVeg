library(dplyr)
library(ggplot2)
library(sp)
load("../data/owens_lai.RData")
load("../data/owens_index.RData")

owens_grid <-



ash_grid <- build_point_grid(filter(owens_lai, zone=="ash_creek"))
sp::coordinates(ash_grid) <- ~ x + y
gridded(ash_grid) <- TRUE
col.num <- 1
d <- colnames(owens_lai)[col.num]

ash_grid %>% select(1, 2, 3, 4, col.num) %>%
  ggplot(aes(x=x, y=y)) +
  geom_tile() +
  coord_fixed()
