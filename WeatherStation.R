library(RgoogleMaps)

setClass("WeatherStation", 
         slots = c(stationdata = "data.frame"),
         prototype = list(
                          stationdata=data.frame(
                                                 City=character(),
                                                 State=character(), 
                                                 Country=character(),
                                                 Id=character(),
                                                 Lat=numeric(),
                                                 Lon=numeric(),
                                                 Distance=numeric())))

.WeatherStation.valid <- function(object) {
  if (nrow(object@stationdata) == 0) {
    return (TRUE)
  }

  if (!is.character(object@stationdata$City)){
    return("City type mismatch")  
  }
  if(!is.character(object@stationdata$State)){
    return("State type mismatch")  
  }
  if(!is.character(object@stationdata$Country)){
    return("Country type mismatch")  
  }
  if( !is.character(object@stationdata$Id)){
    return("Id type mismatch")  
  }
  if( !is.numeric(object@stationdata$Lat)){
    return("Lat type mismatch")  
  }
  if(!is.numeric(object@stationdata$Lon)){
    return("Lon type mismatch")  
  }
  if(!is.numeric(object@stationdata$Distance)){
    return("Distance type mismatch")  
  }
  return(TRUE)
}

setValidity("WeatherStation", .WeatherStation.valid)

"WeatherStation" <- function(
                             stationdata=data.frame(
                                                    City=character(),
                                                    State=character(), 
                                                    Country=character(),
                                                    Id=character(),
                                                    Lat=numeric(),
                                                    Lon=numeric(),
                                                    Distance=numeric())){ 
  new("WeatherStation",stationdata=stationdata)
}

setMethod("subset", "WeatherStation", function(x, dis = 40) {
          r <- x@stationdata[x@stationdata$Distance < dis, ]
          WeatherStation(r)
                             })

.getDf.WeatherStation <- function(x, ...) {
  x@stationdata
}

setGeneric("getDf",function(x){standardGeneric("getDf")})
setMethod("getDf", "WeatherStation", function(x) {
          .getDf.WeatherStation(x)
                             })

setGeneric("saveDf", function(x, filename) standardGeneric("saveDf"))
setMethod("saveDf", "WeatherStation", function(x, filename) {
          saveRDS(x@stationdata, file=filename)
                             })

loadDf <- function(filename) {
  WeatherStation(readRDS(filename))
}

plot.WeatherStation <- function(x, ...){
  gx <- x@stationdata$Lon
  gy <- x@stationdata$Lat
  mymap <- MapBackground(lat=gy, lon=gx) 
  PlotOnStaticMap(mymap, lat=gy, lon=gx,
                  destfile = "weatherstations.png", cex=1.5,pch=20, col="red")
}

setGeneric("plot")
setMethod("plot", c(x = "WeatherStation", y = "missing"), plot.WeatherStation)
