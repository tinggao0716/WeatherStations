require(RgoogleMaps)

setClass("WeatherStation", 
                            slots = c(Lat = "numeric", Lon = "numeric",
  Distance = "numeric"))

setMethod("subset", "WeatherStation", function(x, dis = 40) {
  r <- data.frame(x@Lat, x@Lon, x@Distance)
  colnames(r) <- c("Lat", "Lon", "Distance")
  r[r$Distance < dis, ]
})

