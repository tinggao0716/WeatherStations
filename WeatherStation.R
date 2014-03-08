
WeatherStation <-  setClass("WeatherStation", 
                            slots = c(Lat = "numeric", Lon = "numeric"))
                            #slots = c(City="character", State="character",
                            #Country="character", Id="character",  
                            #Lat = "numeric", Lon = "numeric",
                            #Distance = "numeric"))



plot.WeatherStation <- function(x, ...){
  gx <- x@Lon
  gy <- x@Lat
  plot(gx, gy,  pch = "*", xlab = "Longitude", ylab ="Latitude", ...)
}

setGeneric("plot")

setMethod("plot", c(x = "WeatherStation", y = "missing"), plot.WeatherStation)


setValidity("WeatherStation",
            function(object) {
              messages <- character()
              slots <- c("Lat", "Lon")
              lengths <- sapply(slots,
                                function(what) length(slot(object, what)))
              if(length(unique(lengths))>1)
                messages <- paste("unequal lengths : ",
                                  paste(slots, lengths, sep =":", collapse = ", "))
              if(length(messages))
                messages
              else
                TRUE
            })
