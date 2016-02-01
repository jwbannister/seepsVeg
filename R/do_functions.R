calc_auc <- function(year_data, historical_data){
  complete_year <- patch_year(year_data, historical_data)
  peak_days <- complete_year[complete_year$day>150 & complete_year$day<250, ]
  if (nrow(peak_days)==0) {
    auc_value <- NA
  } else {
    a <- loess(lai ~ day, data=complete_year)
    b <- data.frame(x=complete_year$day, y=a$fitted)
    auc_value <- MESS::auc(b$x, b$y, type="linear")
  }
  auc_value
}

patch_year <- function(year_data, historical_data){
  bottom_patches <- if (min(year_data$day) > 8) seq(1, min(year_data$day), 8) else c()
  top_patches <- if (max(year_data$day) < 357) seq(max(year_data$day)+8, 365, 8) else c()
  patch_days <- c(bottom_patches, top_patches)
  if (length(patch_days)==0){ return(year_data) }
  patch_lai <- c()
  for (i in 1:length(patch_days)){
    d <- patch_days[i]
    tmp <- dplyr::filter(historical_data, (day > (d - 7)) & (day < (d + 7)))
    val <- mean(tmp$lai, na.rm=TRUE)
    patch_lai[i] <- val
  }
  len <- length(patch_days)
  dt <- as.Date(paste0(year_data$year[1], "-01-01"))
  lubridate::yday(dt) <- patch_days
  patch_df <- data.frame(feid=rep(year_data$feid[1], len),
                         lai=patch_lai,
                         day=patch_days,
                         year=rep(year_data$year[1], len))
  year_data <- rbind(year_data, patch_df)
  year_data <- dplyr::arrange(year_data, day)
  year_data
}

build_auc_list <- function(data){
  auc_list <- c()
  for (i in 1:length(data)){
    print(paste0(i, "/", length(data)))
    id_index <- unique(data[[i]]$feid)
    auc_df <- data.frame(matrix(NA, nrow=length(id_index), ncol=length(year_index) + 1))
    colnames(auc_df) <- c("feid", year_index)
    auc_df$feid <- id_index
    for (j in 1:length(id_index)){
      cat("\r", j, "/", length(id_index))
      flush.console()
      historical_data <- dplyr::filter(data[[i]], feid==id_index[j])
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
    auc_list[[i]] <- auc_df
  }
  auc_list
}

build_ecdf_df <- function(data, kern="epanechnikov"){
  data <- data[!is.na(data$auc), ]
  if (!(min(data$auc) < max(data$auc))) {
    ecdf_df <- NA
  } else {
  kernel_ecdf <- sROC::kCDF(data$auc, kernel=kern)
  ecdf_df <- data.frame(x=kernel_ecdf$x, Fhat=kernel_ecdf$Fhat)
  ecdf_df
  }
}

score_quantile <- function(data, cdf_years=year_index){
  quant_list <- list()
  for (i in 1:length(data)){
    print(paste0(i, "/", length(data)))
    id_index <- unique(data[[i]]$feid)
    quant_df <- data.frame(matrix(NA, nrow=length(id_index), ncol=length(year_index) + 1))
    colnames(quant_df) <- c("feid", year_index)
    quant_df$feid <- id_index
    for (j in 1:length(id_index)){
      temp <- dplyr::filter(data[[i]], feid==id_index[j])
      kernel_ecdf <- build_ecdf_df(data=dplyr::filter(temp, year %in% cdf_years))
      for (k in 1:length(year_index)){
        if (is.na(kernel_ecdf)) {
          quant_df[j, k+1] <- NA
        } else if (is.na(temp$auc[k])) {
          quant_df[j, k+1] <- NA
        } else {
        quant_df[j, k+1] <- kernel_ecdf$Fhat[which.min(abs(kernel_ecdf$x - temp$auc[k]))]
        }
      }
    }
    quant_list[[i]] <- quant_df
  }
  quant_list
}

xval_quantile <- function(data, cdf_years=year_index){
  quant_list <- list()
  for (i in 1:length(data)){
    print(paste0(i, "/", length(data)))
    id_index <- unique(data[[i]]$feid)
    quant_df <- data.frame(matrix(NA, nrow=length(id_index), ncol=length(year_index) + 1))
    colnames(quant_df) <- c("feid", year_index)
    quant_df$feid <- id_index
    for (j in 1:length(id_index)){
      temp <- dplyr::filter(data[[i]], feid==id_index[j])
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
    quant_list[[i]] <- quant_df
  }
  quant_list
}

score_low_streak <- function(data, threshold){
  streak_list <- list()
  for (i in 1:length(data)){
    print(paste0(i, "/", length(data)))
    id_index <- unique(data[[i]]$feid)
    streak_df <- data.frame(matrix(NA, nrow=length(id_index), ncol=length(year_index) + 1))
    colnames(streak_df) <- c("feid", year_index)
    streak_df$feid <- id_index
    for (j in 1:length(id_index)){
      temp <- dplyr::filter(data[[i]], feid==id_index[j])
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
    streak_list[[i]] <- streak_df
  }
  streak_list
}

