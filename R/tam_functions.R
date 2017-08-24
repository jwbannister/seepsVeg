
area_quantile <- function(df1, id_col=1, data_col=5){
    df1$tmp.id <- seq(1, nrow(df1), 1)
    for (i in unique(df1[[id_col]])){
        tmp_df <- df1[df1[[id_col]]==i & !is.na(df1[[id_col]]), ]
        for (j in tmp_df$tmp.id){
            kernel_ecdf <- build_ecdf_area(df1=filter(tmp_df, tmp.id!=j), 
                                           data_col=data_col)
            df1[df1$tmp.id==j, ]$quant.score <- 
    kernel_ecdf$Fhat[which.min(abs(kernel_ecdf$x - 
                                   tmp_df[tmp_df$tmp.id==j, ][[data_col]]))]
        }
    }
    df1
}

build_ecdf_area <- function(df1, data_col=5, kern="epanechnikov"){
  if (!(min(df1[[data_col]]) < max(df1[[data_col]]))) {
    ecdf_df <- NA
  } else {
  kernel_ecdf <- sROC::kCDF(df1[[data_col]], kernel=kern)
  ecdf_df <- data.frame(x=kernel_ecdf$x, Fhat=kernel_ecdf$Fhat)
  ecdf_df
  }
}
