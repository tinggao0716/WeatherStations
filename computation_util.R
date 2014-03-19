#######################################
# Plot weather condition given time poitns
# using interpolation
#
# Author: Ting Gao (ting2)
#######################################

plotByInterp <- function(weatherCond, stationId, timeSeq) {
  times <- weatherCond[[1]][["localTimes"]]
  times <- as.POSIXct(times)
  temp <- weatherCond[[stationId]][["temperatureC"]]
  windKm <- weatherCond[[stationId]][["windSpeedKm"]]
  precip <- weatherCond[[stationId]][["precipRateMmPerHr"]]

  f <- approxfun(times, temp)
  g <- approxfun(times, windKm)
  h <- approxfun(times, precip)

  png("timePlot.png")
  par(mfrow=c(3,1))
  plot(times, f(times), col= "green2", main = "Temperature", xlab = "Time", ylab = "Temperature")
  lines(times, f(times), lty=1, lwd = 1, col = "blue")
  points(timeSeq, f(timeSeq), col = "red", pch = 23, lwd = 2)

  plot(times, g(times), col= "green2", main = "Windspeed", xlab = "Time", ylab = "Windspeed")
  lines(times, g(times), lty=1, lwd = 1, col = "blue")
  points(timeSeq, g(timeSeq), col = "red", pch = 23, lwd = 2)

  plot(times, h(times), col= "green2", main = "Precipitation", xlab = "Time", ylab = "Precipitation")
  lines(times, h(times), lty=1, lwd = 1, col = "blue")
  points(timeSeq, h(timeSeq), col = "red", pch = 23, lwd = 2)
  dev.off()
}

