setwd("E:/stats290/WeatherStations/Ting")
source("WeatherStation.R") 
source("q1.R")


location <- c(37.786289,-122.405234)
dist_km <- 3
filename <- paste(location[1], "_", location[2], 
                  "_", dist_km, ".csv", sep="")
stationdata <- Stations(location, dist_km)
#write.table(stationdata, "filename.csv")
#stationdata <- read.csv("filename.csv")


sta1 <- WeatherStation(Lat = stationdata$Lat, Lon = stationdata$Lon)
method.skeleton("plot", c("WeatherStation", "missing"), 
                file = stdout())
plot(sta1)


## examples of validity computations
sta2<-sta1

## let's mess the object up:
sta2@Lon <- sta2@Lon[1:50] #wrong length
sta2@Lon <- sample(sta2@Lon, size = 230, replace = TRUE) # and not monotone
validObject(sta2)
