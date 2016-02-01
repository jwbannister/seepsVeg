makePNG <- function(plt, filename, ht=6, wt=6, ppi=300){
  png(filename, width=wt*ppi, height=ht*ppi, res=ppi)
  print(plt)
  dev.off()
}
