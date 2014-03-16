#Ting's code

#load station data
source("location_dist.R")
location <- c(37.786289,-122.405234)
dist_km <- 10
filename <- paste(location[1], "_", location[2], 
                  "_", dist_km, ".csv", sep="")
stationdata <- read.csv(filename)


# plot stations defined by S4 class 
source("WeatherStation.R") 
WStation <- WeatherStation(Lat = stationdata$Lat, Lon = stationdata$Lon)
                           #dist = stationdata$Distance)
method.skeleton("plot", c(x = "WeatherStation", y = "missing"),
                file = stdout())
plot(WStation)


## validity check
sta2<-WStation

## let's mess the object up:
sta2@Lon <- sta2@Lon[1:50] #wrong length
sta2@Lon <- sample(sta2@Lon, size = 230, replace = TRUE) # and not monotone
validObject(sta2)
