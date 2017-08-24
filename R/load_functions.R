#' Read and concatenate multiple DBF files.
#'
#' \code{load_dbfs} loads a multiple DBF (dBase) files and joins them on a
#'  common ID variable into a dataframe object.
#'
#'  @param file Vector of character strings. DBF files to be read,
#'    including path.
#'  @return Dataframe with three columns: point ID, x coordinate,
#'    and y coordinate
load_dbfs <- function(files=NULL){
  for (i in files){
    temp <- foreign::read.dbf(file=i)
    if (i==files[1]){
      built_df <- temp
    } else{
      built_df <- dplyr::inner_join(built_df, temp)
    }
  }
  built_df
}

#' Build an index of dates for which LAI images are available.
#'
#' \code{build_dates_index} looks through LAI data in the format as supplied
#'  for Owens Lake and builds a list of all the dates for which LAI images
#'  were obtained.
#'
#'  @param data Data frame. LAI data.
#'  @return Returns a vector of dates.
build_dates_index <- function(data){
  temp <- colnames(data)
  temp <- gsub("d", "", temp)
  temp <- as.Date(temp, "%Y%m%d")
  temp
}

#' Split a large dataframe into multiple smaller data frames.
#'
#' \code{split_df} splits a dataframe into n (nearly) equal pieces, all
#' pieces containing all columns of the original data frame. If the number
#' of rows in the original dataframe is not evenly divisibile by n, the nth
#' dataframe will contain the remainder rows.
#'
#'  @param data Data frame.
#'  @param n Integer. Number of divisions.
#'  @return Returns a list of dataframes. All dataframes in list will
#'          comprise the original data frame.
split_df <- function(data, n){
  rownames(data) <- c(1:nrow(data))
  split_list <- list()
  split_size <- nrow(data) %/% n
  for (i in 1:(n - 1)){
    split_list[[i]] <- data[(((i - 1) * split_size) + 1):(i * split_size), ]
  }
  split_list[[n]] <- data[(((n - 1) * split_size) + 1):nrow(data), ]
  split_list
}
