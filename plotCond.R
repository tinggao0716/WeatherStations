plotCond <- function(StationCond, condition) {
  
  #get dimensions from RgoogleMaps
  mymap <- MapBackground(lat=StationCond$Lat, lon=StationCond$Lon)
  
  #make background using ggmap
  myggmap <- get_map(location = c(lon = mymap$lon.center, lat = mymap$lat.center), 
                      maptype = "roadmap", zoom = mymap$zoom)
  
  #make ggplot
  myggplot <- ggmap(myggmap) + 
              geom_point(data=StationCond, 
                         aes_string(x="Lon", y="Lat", color=condition), 
                         size=5)   
  
  #output to pdf
  pdf(paste(condition, ".pdf"))
  plot(myggplot)
  dev.off()
}
