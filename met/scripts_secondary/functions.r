# functions related to CIMIS station data

resetDailyCumStartDate <- function(data, new_start){
  new_date <- as.Date(new_start, "%m/%d/%Y")
  data <- filter(data, date>new_date)
  data <- data %>% group_by(station) %>% arrange(date) %>%
    mutate(delta=precip-mean(precip), cum.delta=0)
  pb <- txtProgressBar(min=0, max=nrow(data), style=3, width=80)
  tick <- 0
  for (i in unique(data$station)){
    for (j in 2:nrow(data[data$station==i, ])){
      data[data$station==i, ]$cum.delta[j] <- 
        sum(data[data$station==i, ]$delta[1:j])
      tick <- tick + 1
      setTxtProgressBar(pb, tick)
    }
  }
  close(pb)
  plt <- ggplot(data, aes(x=date, y=cum.delta)) +
    geom_path(aes(color=station))
  plt
}

makePNG <- function(plt, filename, ht=6, wt=6, ppi=300){
  png(paste0("../output/", filename, ".png"), width=wt*ppi, height=ht*ppi, res=ppi)
  print(plt)
  dev.off()
}

parseNOAA <- function(x){
  # this function is intended to parse output data from NOAA web API calls
  parsed <- content(x, as="parsed", type="application/json")$results
  headers <- names(parsed[[1]])
  df <- data.frame(matrix(unlist(parsed), ncol=length(headers), byrow=TRUE, 
                          dimnames=list(seq(1, length(parsed), 1), headers)),
                   stringsAsFactors=FALSE)
  df
}

loadCDECMonthlyData <- function(id, variable){
  df <- read.csv(paste0("../data/cdec/", id, "_", variable, "_monthly.csv"), skip=3)
  colnames(df) <- gsub("X", "", colnames(df))
  colnames(df) <- gsub("[.]", "", colnames(df))
  df <- filter(df, station!="'End of Data")
  df$station <- as.character(df$station)
  df$year <- as.character(df$year)
  df <- select(df, -sensor, -month)
  df <- melt(df, id.vars=c("station", "year"), 
                             variable.name="month", value.name=variable)
  df <- filter(df, !is.na(year))
  df$month <- as.character(df$month)
  df$ref.date <- as.Date(paste0(df$month, "/15/", df$year), 
                                         "%m/%d/%Y")
  df[ ,4] <- as.numeric(df[ , 4])
  df <- df[!is.na(df[ ,4]), ]
  df
}