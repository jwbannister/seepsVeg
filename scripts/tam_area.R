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
tam_summary$quant <- rep(NA, nrow(tam_summary))
tam_summary$tmp.id <- seq(1, nrow(tam_summary), 1)
for (i in unique(tam_summary$name)){
    tmp_df <- tam_summary[tam_summary$name==i & !is.na(tam_summary$name), ]
    for (j in tmp_df$tmp.id){
        kernel_ecdf <- build_ecdf_area(df1=filter(tmp_df, tmp.id!=j), 
                                       data_col=5)
        tam_summary[tam_summary$tmp.id==j, ]$quant <- 
            kernel_ecdf$Fhat[which.min(abs(kernel_ecdf$x - 
                                           tmp_df[tmp_df$tmp.id==j, ][[5]]))]
    }
}
tam_summary$year <- as.integer(gsub("f", "", tam_summary$year))
tam_wide_quant <- tam_summary %>% select(name, year, quant) %>% 
    spread(year, quant)
tam_wide_area <- tam_summary %>% select(name, year, tam.ratio) %>% 
    spread(year, tam.ratio)
write.csv(tam_wide_quant, file="~/Desktop/output/tam_quant.csv", row.names=F)
write.csv(tam_wide_area, file="~/Desktop/output/tam_prcnt_area.csv", row.names=F)

year_index <- unique(tam_summary$year)

split_tam <- vector(mode="list", length=1)
split_tam[[1]] <- tam_summary
streak_df_0.1 <- score_low_streak(split_tam, threshold=0.1, id_col="name")[[1]]
write.csv(streak_df_0.1, file="~/Desktop/output/tam_streak_01.csv", row.names=F)
streak_df_0.2 <- score_low_streak(split_tam, threshold=0.2, id_col="name")[[1]]
write.csv(streak_df_0.2, file="~/Desktop/output/tam_streak_02.csv", row.names=F)
streak_df_0.3 <- score_low_streak(split_tam, threshold=0.3, id_col="name")[[1]]
write.csv(streak_df_0.3, file="~/Desktop/output/tam_streak_03.csv", row.names=F)
