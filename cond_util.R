getCond <- function(startDt, endDt, stationdata) {  
  #get number of stations
  numStations <- nrow(stationdata)
  
  #make date sequence
  dateSeq <- seq(as.Date(startDt),to=as.Date(endDt),by="days")
    
  #initialize weatherList
  weatherList <- list()
  length(weatherList) <- numStations

  #create column names
  columnNames <- c("lYr", "lMonth", "lDay", "lHr", "lMin", 
                  "temperatureC", "dewPointC", "humidity", "windSpeedKm", 
                   "windGustKm", "pressureMb", "precipRateMmPerHr", "precipTotalMm")
    
  #add values to weatherList
  for (i in 1:numStations) {
    conditionData <- data.frame(matrix(ncol = length(columnNames), nrow = 0))
    #get values from xml
    for (j in 1:length(dateSeq)) {
      webid <- paste("http://api.wunderground.com/api/b340be89948f3793/history_", 
                     format(as.Date(dateSeq[j]), "%Y%m%d"), "/q/pws:", 
                     stationdata[i, "Id"], ".xml", sep="")      
      doc <- xmlTreeParse(file=webid, useInternal=TRUE)
      top <- xmlRoot(doc)  
      newData <- data.frame(
        unlist(xpathApply(top, "//observation/date/year", xmlValue)),
        unlist(xpathApply(top, "//observation/date/mon", xmlValue)),
        unlist(xpathApply(top, "//observation/date/mday", xmlValue)),
        unlist(xpathApply(top, "//observation/date/hour", xmlValue)),
        unlist(xpathApply(top, "//observation/date/min", xmlValue)),
        unlist(xpathApply(top, "//observation/tempm", xmlValue)),
        unlist(xpathApply(top, "//observation/dewptm", xmlValue)),
        unlist(xpathApply(top, "//observation/hum", xmlValue)),
        unlist(xpathApply(top, "//observation/wspdm", xmlValue)),
        unlist(xpathApply(top, "//observation/wgustm", xmlValue)),
        unlist(xpathApply(top, "//observation/pressurem", xmlValue)),
        unlist(xpathApply(top, "//observation/precip_ratem", xmlValue)),
        unlist(xpathApply(top, "//observation/precip_totalm", xmlValue))
      )
      conditionData <- rbind(conditionData, newData)
    }
    #assign column names
    colnames(conditionData) <- columnNames
    #update weatherList
    weatherList[[i]] <- conditionData
  } 
  #name weatherList based on weather station Id
  names(weatherList) <- paste(stationdata[1:numStations, "Id"])
  
  #process weatherList
  for (i in 1:length(weatherList)) {
    localTimes <- vector()
    if (nrow(weatherList[[i]]) > 0) {
      #convert variables to numeric
      for (j in 1:ncol(weatherList[[i]])) {
        weatherList[[i]][,j] <- as.numeric(levels(weatherList[[i]][,j]))[weatherList[[i]][,j]]
      }    
      #combine date and time into single variable
      times <- paste(weatherList[[i]][["lYr"]], "-", 
                     weatherList[[i]][["lMonth"]], "-", 
                     weatherList[[i]][["lDay"]], " ", 
                     weatherList[[i]][["lHr"]], ":", 
                     weatherList[[i]][["lMin"]], sep="" )
      localTimes <- as.POSIXct(times)
      
    }
    #remove old variables for date and time
    weatherList[[i]] <- weatherList[[i]][, !(names(weatherList[[i]]) %in% 
                                               c("lYr", "lMonth", "lDay", "lHr", "lMin"))]
    #insert new variable for date and time
    weatherList[[i]] <- cbind(localTimes, weatherList[[i]])
  }  
  
  return(weatherList)
}

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

plotCond <- function(StationCond, condition) {
  
  #get dimensions from RgoogleMaps
  mymap <- MapBackground(lat=StationCond$Lat, lon=StationCond$Lon)
  
  #make background using ggmap
  myggmap <- get_map(location = c(lon = mymap$lon.center, lat = mymap$lat.center), 
                      maptype = "roadmap", zoom = mymap$zoom)
  
  #make ggplot
  myggplot <- ggmap(myggmap) + 
              geom_point(data=StationCond, 
                         aes_string(x="Lon", y="Lat", color=condition), 
                         size=5)   
  
  #output to pdf
  pdf(paste(condition, ".pdf"))
  plot(myggplot)
  dev.off()
}
