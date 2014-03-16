library(RgoogleMaps)

WeatherStation <-  setClass("WeatherStation", 
                            slots = c(Lat = "numeric", Lon = "numeric", Distance = "integer"))

plot.WeatherStation <- function(x, ...){
  gx <- x@Lon
  gy <- x@Lat
  mymap <- MapBackground(lat=gy, lon=gx) 
  PlotOnStaticMap(mymap, lat=gy, lon=gx,
                  destfile = "weatherstations.png", cex=1.5,pch=20, col="red")
}

setGeneric("plot")
setMethod("plot", c(x = "WeatherStation", y = "missing"), plot.WeatherStation)

setMethod("subset", "WeatherStation", function(x, dis = 40) {
  r <- data.frame(x@Lat, x@Lon, x@Distance)
  colnames(r) <- c("Lat", "Lon", "Distance")
  r[r$Distance < dis, ]
})


setValidity("WeatherStation",
            function(object) {
              messages <- character()
              slots <- c("Lat", "Lon")
              lengths <- sapply(slots,
                                function(what) length(slot(object, what)))
              if(length(unique(lengths))>1)
                messages <- paste("unequal lengths : ",
                                  paste(slots, lengths, sep =":", collapse = ", "))
              if(length(messages))
                messages
              else
                TRUE
            })
