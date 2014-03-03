source("Stations.R")

stations <- Stations$new() 
results <- xmlEventParse(
    file="http://api.wunderground.com/api/f8bbd9ebde0769a6/geolookup/q/CA/San_Francisco.xml",
    branches = stations$saxHandler())

print(head(stations$stations()))

stations$plot("station_pos.png")

