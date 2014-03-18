library(RgoogleMaps)

setClass("WeatherStation", 
         slots = c(Lat = "numeric", Lon = "numeric", Distance = "integer"))

.WeatherStation.valid <- function(object) {
  len <- length(object@Lat)
  if(len != length(object@Lon) || len != length(object@Distance)) {
    return("length mismatch")
  }
  return(TRUE)
}

setValidity("WeatherStation", .WeatherStation.valid)

"WeatherStation" <- function(Lat=double(), Lon=double(), Distance=integer()){
  new("WeatherStation",Lat=as.numeric(Lat),Lon = as.numeric(Lon), Distance=as.integer(Distance))
}

setMethod("subset", "WeatherStation", function(x, dis = 40) {
          r <- data.frame(x@Lat, x@Lon, x@Distance)
          colnames(r) <- c("Lat", "Lon", "Distance")
          r <- r[r$Distance < dis, ]
          WeatherStation(r$Lat, r$Lon, r$Distance)
         })

plot.WeatherStation <- function(x, ...){
  gx <- x@Lon
  gy <- x@Lat
  mymap <- MapBackground(lat=gy, lon=gx) 
  PlotOnStaticMap(mymap, lat=gy, lon=gx,
                  destfile = "weatherstations.png", cex=1.5,pch=20, col="red")
}

setGeneric("plot")
setMethod("plot", c(x = "WeatherStation", y = "missing"), plot.WeatherStation)

.getDf.WeatherStation <- function(x, ...) {
  r <- data.frame(x@Lat, x@Lon, x@Distance)
  colnames(r) <- c("Lat", "Lon", "Distance")
  r
}

setGeneric("getDf",function(x){standardGeneric("getDf")})
setMethod("getDf", "WeatherStation", function(x) {
          .getDf.WeatherStation(x)
                  })
