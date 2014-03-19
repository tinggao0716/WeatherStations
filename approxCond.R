approxCond <- function(inputTime, stationdata, weatherCond) {
  #convert format of inputTime
  inputTime <- as.POSIXct(inputTime)
  
  #find number of stations
  numStations <- length(weatherCond)
  
  #find number of variables
  numVars <- ncol(weatherCond[[1]])
  
  #initialize subsetCond
  subsetCond <- data.frame(matrix(ncol = numVars, nrow = numStations))
  names(subsetCond) <- names(weatherCond[[1]])
  
  #approximate values at inputTime
  for (i in 1:numStations) {
    if (nrow(weatherCond[[i]]) > 0) {
      for (j in 1:numVars) {
        if (names(weatherCond[[i]])[[j]]!="localTimes") {
          f <- approxfun(weatherCond[[i]][,"localTimes"], weatherCond[[i]][,j])
          subsetCond[[i,j]] <- f(inputTime)
        }
      }
    }
  }
  
  #remove the time variable
  subsetCond <- subsetCond[, names(subsetCond)!="localTimes"]
  
  #combine with station location
  subCondPws <- cbind(stationdata[, c("Id", "Lat", "Lon")], subsetCond)
  
  #remove stations with missing data
  subCondPws <- subCondPws[rowSums(is.na(subCondPws)) != ncol(subsetCond),]

  #handle invalid data
  #temperature
  naRows <- which((subCondPws[, "temperatureC"] < -50)
                  | (subCondPws[, "temperatureC"] > 50))
  if (length(naRows) > 0) {subCondPws[naRows, "temperatureC"] <- NA} 
  #dew point
  naRows <- which((subCondPws[, "dewPointC"] < -50)
                  | (subCondPws[, "dewPointC"] > 50))
  if (length(naRows) > 0) {subCondPws[naRows, "dewPointC"] <- NA} 
  #humidity
  naRows <- which((subCondPws[, "humidity"] <= 0)
                  | (subCondPws[, "humidity"] > 100))
  if (length(naRows) > 0) {subCondPws[naRows, "humidity"] <- NA}  
  #wind speed
  naRows <- which((subCondPws[, "windSpeedKm"] < 0))
  if (length(naRows) > 0) {subCondPws[naRows, "windSpeedKm"] <- NA}   
  #wind gust
  naRows <- which((subCondPws[, "windGustKm"] < 0))
  if (length(naRows) > 0) {subCondPws[naRows, "windGustKm"] <- NA}
  #pressure
  naRows <- which((subCondPws[, "pressureMb"] < 0))
  if (length(naRows) > 0) {subCondPws[naRows, "pressureMb"] <- NA}   
  #precipitation rate
  naRows <- which((subCondPws[, "precipRateMmPerHr"] < 0))
  if (length(naRows) > 0) {subCondPws[naRows, "precipRateMmPerHr"] <- NA}  
  #precipitation total
  naRows <- which((subCondPws[, "precipTotalMm"] < 0))
  if (length(naRows) > 0) {subCondPws[naRows, "precipTotalMm"] <- NA}    
  
  return(subCondPws)
}
