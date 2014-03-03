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

  stations = function() {
    colnames(stationdata) <<-
       c("City", "State", "Country", "Icao", "Id", "Lat", "Lon") 
    stationdata
  },

  plot = function(imgname = "positions.png") {
    # TODO(tinggao): Move the settings to the end of station data extraction
    colnames(stationdata) <<-
       c("City", "State", "Country", "Icao", "Id", "Lat", "Lon") 

    require(ggplot2)
    require(scales)
    png(filename = imgname)
    p <- ggplot(sta, aes(x=Lon, y=Lat, label=Icao)) +
        geom_point(size=2) + geom_text()
    print(p)
    dev.off()
  },

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
          as.numeric(xmlValue(node[["lat"]])),
          as.numeric(xmlValue(node[["lon"]])))
      stationdata <<- rbind(stationdata, newdata)
    }
    c(station = xmlParserContextFunction(station))
  }
)



