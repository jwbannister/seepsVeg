devtools::load_all()
library(tidyverse)

path <- path.expand("~/airsci/owens/seeps_springs/aug_2017_analysis/")
feid_df <- rgdal::readOGR(paste0(path, "SS_TAM"), "SS_TAM")@data
names(feid_df) <- tolower(names(feid_df))
df1 <- feid_df %>% select(-objectid_1, -objectid, -objectid_2, -objectid_3, 
                          -olgep_name, -shape_leng, -shape_le_1, -shape_le_2, 
                          -shape_area, -fid_1)
tam_summary <- feid_df %>% gather(year, class, f1985:f2015) %>%
    group_by(name, year) %>%
    summarize(all.pixels=length(class), 
              tam.pixels=sum(class=='123'), 
              tam.ratio=sum(class=='123')/length(class))

tam_summary <- tam_summary %>% filter(!is.na(name))
tam_summary$quant.score <- rep(NA, nrow(tam_summary))
tam_summary$tmp.id <- seq(1, nrow(tam_summary), 1)
for (i in unique(tam_summary$name)){
    tmp_df <- tam_summary[tam_summary$name==i & !is.na(tam_summary$name), ]
    for (j in tmp_df$tmp.id){
        kernel_ecdf <- build_ecdf_area(df1=filter(tmp_df, tmp.id!=j), 
                                       data_col=5)
        tam_summary[tam_summary$tmp.id==j, ]$quant.score <- 
            kernel_ecdf$Fhat[which.min(abs(kernel_ecdf$x - 
                                           tmp_df[tmp_df$tmp.id==j, ][[5]]))]
    }
}
tam_quant <- area_quantile(tam_summary) 
write.csv(tam_quant, file="~/Desktop/tam_quantiles.csv", row.names=F)


split_tam <- vector(mode="list", length=1)
split_tam[[1]] <- tam_quant
streak_list <- score_low_streak(split_tam, threshold=0.1, id_col="area")
split_streak <- lapply(streak_list, reshape2::melt, id.vars=c("feid"),
                    variable.name="year", value.name="streak")
save(split_streak, file="./data-analysis/streak.RData")
