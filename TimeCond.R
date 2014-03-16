##Ben code
source("getCond.R")
startDt <- "2014-03-07"
endDt <- "2014-03-09"
weatherCond <- getCond(startDt, endDt, stationdata)

head(weatherCond[[1]])
weatherCond[["KCASANFR231"]][["windKm"]]


##Ting time series weather condition interpolation

for (j in seq(1:length(weatherCond))){
  filename <- paste(stationdata$Id[j], ".csv") 
  write.csv(weatherCond[[j]] , filename, row.names=FALSE)
}


times<- paste(weatherCond[[1]][["lYr"]], "-", 
              weatherCond[[1]][["lMonth"]], "-", 
              weatherCond[[1]][["lDay"]], " ", 
              weatherCond[[1]][["lHr"]], ":", 
              weatherCond[[1]][["lMin"]], sep="" )
times <- as.POSIXct(times)

temp <- weatherCond[["KCASANFR231"]][["tempC"]]
temp <- as.numeric(levels(temp))[temp]
#times <- strptime(times, "%Y-%m-%d %H:%M")

windKm <- weatherCond[["KCASANFR231"]][["windKm"]]
windKm <- as.numeric(levels(windKm))[windKm]
precip <- weatherCond[["KCASANFR231"]][["precip_rateMm"]]
precip <- as.numeric(levels(precip))[precip]

f <- approxfun(times, temp)
g <- approxfun(times, windKm)
h <- approxfun(times, precip)

Inptime <- "2014-03-08 00:00"
Inptime <- as.POSIXct(Inptime)

par(mfrow=c(3,1))
plot(times, f(times), col= "green2", main = "Temperature", xlab = "Time", ylab = "Temperature")
lines(times, f(times), lty=1, lwd = 1, col = "blue")
points(Inptime, f(Inptime), col = "red", pch = 23, lwd = 2)

plot(times, g(times), col= "green2", main = "Windspeed", xlab = "Time", ylab = "Windspeed")
lines(times, g(times), lty=1, lwd = 1, col = "blue")
points(Inptime, g(Inptime), col = "red", pch = 23, lwd = 2)

plot(times, h(times), col= "green2", main = "Precipitation", xlab = "Time", ylab = "Precipitation")
lines(times, h(times), lty=1, lwd = 1, col = "blue")
points(Inptime, h(Inptime), col = "red", pch = 23, lwd = 2)



#plot(times, temp, xaxt="n")
#axis.POSIXct(1,at=times,labels=format(times,"%b-%d"),las=2)
#axis.POSIXct(1, at=times, labels=format(times, "%m/%d"))
#lines(times, temp, lty=1, lwd=2)

