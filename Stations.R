library(XML)

Stations <- setRefClass("Stations",
    fields = list(stationdata = "data.frame"))

Stations$methods(
  initialize=function() {
    .self$stationdata <- data.frame(
        City=character(),
        State=character(),
        Country=character(),
        Icao=character(),
        Id=character(),
        Lat=numeric(),
        Lon=numeric())
  },

  stations = function() stationdata,
  
  saxHandler = function() {
    stationdata <<- data.frame(
        City=character(),
        State=character(),
        Country=character(),
        Icao=character(),
        Id=character(),
        Lat=numeric(),
        Lon=numeric())
    station <- function(context, node) {
      newdata <- data.frame(
          xmlValue(node[["city"]]),
          xmlValue(node[["state"]]),
          xmlValue(node[["country"]]),
          xmlValue(node[["icao"]]),
          xmlValue(node[["id"]]),
          xmlValue(node[["lat"]]),
          xmlValue(node[["lon"]]))
      stationdata <<- rbind(stationdata, newdata)
    }
    colnames(stationdata) <<- c("city", "state", "country", "icao", "id", "lat", "lon")
    c(station = xmlParserContextFunction(station))
  }
)



