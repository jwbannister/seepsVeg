apply_auc <- function(data){
  id_index <- unique(data$feid)
  auc_df <- data.frame(matrix(NA, nrow=length(id_index), ncol=length(year_index) + 1))
  colnames(auc_df) <- c("feid", year_index)
  auc_df$feid <- id_index
  for (j in 1:length(id_index)){
    cat("\r", j, "/", length(id_index))
    flush.console()
    historical_data <- dplyr::filter(data, feid==id_index[j])
    for (k in 1:length(year_index)){
      year_data <- dplyr::filter(historical_data, feid==id_index[j],
                                 year==year_index[k])
      year_data <- year_data[!is.na(year_data$lai), ]
      if (nrow(year_data)==0){
        auc_df[j, k+1] <- NA
      } else {
        auc_df[j, k+1] <- calc_auc(year_data, historical_data)
      }
    }
  }
  auc_df[auc_df<=0] <- 0.00001
  auc_df
}

apply_quant <- function(data, cdf_years=year_index){
  id_index <- unique(data$feid)
  quant_df <- data.frame(matrix(NA, nrow=length(id_index), ncol=length(year_index) + 1))
  colnames(quant_df) <- c("feid", year_index)
  quant_df$feid <- id_index
  for (j in 1:length(id_index)){
    cat("\r", j, "/", length(id_index))
    flush.console()
    temp <- dplyr::filter(data, feid==id_index[j])
    for (k in 1:length(year_index)){
     xval_years <- cdf_years[cdf_years!=year_index[k]]
      kernel_ecdf <- build_ecdf_df(data=dplyr::filter(temp, year %in% xval_years))
      if (is.na(kernel_ecdf)) {
        quant_df[j, k+1] <- NA
      } else if (is.na(temp$auc[k])) {
        quant_df[j, k+1] <- NA
      } else {
        quant_df[j, k+1] <- kernel_ecdf$Fhat[which.min(abs(kernel_ecdf$x - temp$auc[k]))]
      }
    }
  }
  quant_df
}

apply_score <- function(data, threshold){
  id_index <- unique(data$feid)
  streak_df <- data.frame(matrix(NA, nrow=length(id_index), ncol=length(year_index) + 1))
  colnames(streak_df) <- c("feid", year_index)
  streak_df$feid <- id_index
  for (j in 1:length(id_index)){
    cat("\r", j, "/", length(id_index))
    flush.console()
    temp <- dplyr::filter(data, feid==id_index[j])
    temp$year <- as.numeric(temp$year)
    temp <- temp[sort.int(temp$year, index.return=TRUE)$ix, ]
    for (k in 1:length(year_index)){
      n <- 0
      for (m in k:1){
        if (is.na(temp$quant[m])){
          break
        } else if (temp$quant[m] < threshold){
          n <- n + 1
        } else {
          break
        }
      }
      streak_df[j, k+1] <- n
    }
  }
  streak_df
}
