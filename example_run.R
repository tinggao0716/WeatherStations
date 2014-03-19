source("WeatherStation.R")

createTestDf <- function(){
  data.frame(
             City= c("A", "BC"),
             State=c("CA", "IL"),
             Country=c("Santa Clara", "Evanston"),
             Id=c("af4", "safsAS"),
             Lat=c(12.3, 32.241),
             Lon=c(34.5, 108.1),
             Distance=c(2,5),
             stringsAsFactors=FALSE)
}

df <- createTestDf()
ws<-WeatherStation(df)

png("ws.png")
plot(ws)
dev.off()
