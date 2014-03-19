# Ting's code: create table for all PWS in a region specified by location and distance
library(XML)

source("WeatherStation.R")

Stations <- function(location, dist_km){
  lat <- location[1]
  lon <- location[2]
  webid <- paste("http://api.wunderground.com/api/f8bbd9ebde0769a6/geolookup/q/", lat, ",", lon, ".xml", sep="")
  doc <- xmlTreeParse(file=webid, useInternal=TRUE)
  top <- xmlRoot(doc)
  stationdata <- data.frame(
    City = unlist(xpathApply(top, "//pws/station/city", xmlValue)),
    State = unlist(xpathApply(top, "//pws/station/state", xmlValue)),
    Country = unlist(xpathApply(top, "//pws/station/country", xmlValue)),
    Id = unlist(xpathApply(top, "//pws/station/id", xmlValue)),
    Lat = as.numeric(unlist(xpathApply(top, "//pws/station/lat", xmlValue))),
    Lon = as.numeric(unlist(xpathApply(top, "//pws/station/lon", xmlValue))),
    Distance = as.numeric(unlist(xpathApply(top, "//distance_km", xmlValue))),
    stringsAsFactors=FALSE)

  results <- stationdata[stationdata$Distance <= dist_km, ]

  WeatherStation(results)
}

