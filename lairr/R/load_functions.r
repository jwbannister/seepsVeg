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
