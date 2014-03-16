# Ting's code: create table for all PWS in a region specified by location and distance
library(XML)

Stations <- function(location, dist_km){
  lat <- location[1]
  lon <- location[2]
  webid <- paste("http://api.wunderground.com/api/f8bbd9ebde0769a6/geolookup/q/", lat, ",", lon, ".xml", sep="")
  doc <- xmlTreeParse(file=webid, useInternal=TRUE)
  top <- xmlRoot(doc)
  stationdata <- data.frame(
    unlist(xpathApply(top, "//pws/station/city", xmlValue)),
    unlist(xpathApply(top, "//pws/station/state", xmlValue)),
    unlist(xpathApply(top, "//pws/station/country", xmlValue)),
    unlist(xpathApply(top, "//pws/station/id", xmlValue)),
    as.numeric(unlist(xpathApply(top, "//pws/station/lat", xmlValue))),
    as.numeric(unlist(xpathApply(top, "//pws/station/lon", xmlValue))),
    as.numeric(unlist(xpathApply(top, "//distance_km", xmlValue))))
  colnames(stationdata) <-
    c("City", "State", "Country", "Id", "Lat", "Lon", "Distance")
  results <- stationdata[stationdata$Distance <= dist_km, ]
}


