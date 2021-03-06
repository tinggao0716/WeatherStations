#######################################
# Example end-to-end run
#
# Author: Ting Gao (ting2)
#######################################

library(XML)
library(ggplot2)
library(gridExtra)
library(RgoogleMaps)
library(ggmap)

# get weather stations object (S4 class)
source("station-from-web.R")
location <- c(37.786289,-122.405234)
dist_km <- 30
filename <- paste("./data/", location[1], "_", location[2], 
                                    "_", dist_km, ".rds", sep="")
#df <- Stations(location, dist_km)
#PwsStations <- WeatherStations(df)
#saveDf(PwsStations, filename)
ws <- loadDf(filename)
cat("Weather stations got")

# Plot stations
png("ws.png")
plot(ws)
dev.off()
cat("Weather stations plotted")

# get weather conditions for station list
source("cond_util.R")
startDate <- "2014-03-12"
endDate <- "2014-03-13"
#weatherConditions <- getCond(startDate, endDate, getDf(ws))
cond_file <- "./data/conditions.rds"
#saveRDS(weatherConditions, cond_file)
wc <- readRDS(cond_file)
cat("Conditions obtained")
                                                        
#approximate station weather data for a particular time 
Inptime <- "2014-03-12 12:00"                           
InptimeCond <- approxCond(Inptime, getDf(ws), wc)
cat("approximate conditions obtained")

#list out conditions for reference                      
names(InptimeCond)[!(names(InptimeCond) %in% c("Id", "Lat", "Lon"))]

#plot a weather condition for all stations              
#plot will be in pdf                                    
plotCond(InptimeCond, "temperatureC")
plotCond(InptimeCond, "humidity")
cat("Conditions plotted")

# Plot for question 5
startDt <- "2014-03-15"
endDt <- "2014-03-17"
#weatherCond5 <- getCond(startDt, endDt, getDf(ws))
cond_file <- "./data/conditions_5.rds"
#saveRDS(weatherCond5, cond_file)
wc5 <- readRDS(cond_file)
cat("Conditions for question 5 obtained")

source("computation_util.R")
stationId <- getDf(ws)$Id[1]
timeSeq <- seq(as.POSIXct(startDt),to=as.POSIXct(endDt),by="hour")
plotByInterp(wc5, stationId, timeSeq)

