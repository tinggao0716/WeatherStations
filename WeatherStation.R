library(RgoogleMaps)

WeatherStation <-  setClass("WeatherStation", 
                            slots = c(Lat = "numeric", Lon = "numeric"))
                                     # dist = "integer"))

#Zoomingin <- setClass("Zoomingin", slots=c(zoomdist = "integer"))

plot.WeatherStation <- function(x, ...){
  gx <- x@Lon
  gy <- x@Lat
  mymap <- MapBackground(lat=gy, lon=gx) 
  PlotOnStaticMap(mymap, lat=gy, lon=gx,
                  destfile = "weatherstations.png", cex=1.5,pch=20, col="red")
}

setGeneric("plot")
setMethod("plot", c(x = "WeatherStation", y = "missing"), plot.WeatherStation)


#subset.WeatherStation <- function(x, Dist, ...){
#  subset(x, x@dist<Dist)
#}
#setGeneric("subset")
#setMethod("subset", c(x = "WeatherStation", Dist = "Zoomingin"), subset.WeatherStation )

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
