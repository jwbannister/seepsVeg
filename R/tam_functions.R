
build_ecdf_area <- function(df1, data_col=5, kern="epanechnikov"){
  if (!(min(df1[[data_col]]) < max(df1[[data_col]]))) {
    ecdf_df <- NA
  } else {
  kernel_ecdf <- sROC::kCDF(df1[[data_col]], kernel=kern)
  ecdf_df <- data.frame(x=kernel_ecdf$x, Fhat=kernel_ecdf$Fhat)
  ecdf_df
  }
}
