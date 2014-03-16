# function for 1
source("location_dist.R")

# example for 1
location <- c(37.786289,-122.405234)
dist_km <- 10
filename <- paste(location[1], "_", location[2], 
                  "_", dist_km, ".csv", sep="")
stationdata <- Stations(location, dist_km)
write.csv(stationdata, filename, row.names=FALSE)


# function for 2
source("WS2.R") 
ws <- new("WeatherStation", Lat = stationdata$Lat, Lon = stationdata$Lon,
Distance = stationdata$Distance)

ss <- subset(ws, dis = 3)
#WStation <- WeatherStation(Lat = stationdata$Lat, Lon = stationdata$Lon,
#                           dist = stationdata$Distance)
##method.skeleton("plot", c(x = "WeatherStation", y = "missing"),
##                file = stdout())
##plot(WStation)
#
#ss <- subset(WStation)



