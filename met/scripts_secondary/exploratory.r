a <- filter(cimis_df, station.name=="Owens Lake North")
b <- filter(cimis_df, station.name=="Owens Lake South")


ggplot(a, aes(x=month.year, y=total.precip)) +
  geom_line()
