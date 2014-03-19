\name{getCond}
\alias{getCond}

\title{
get weather conditions
}
\description{
Returns the weather conditions for regional PWS.
}
\usage{
getCond(startDt, endDt, stationdata)
}

\arguments{
  \item{startDt}{
the start date (in the format "YYYY-MM-DD")
}
  \item{endDt}{
the end date (in the format "YYYY-MM-DD")
}
  \item{stationdata}{
a data frame of regional PWS
}
}

\details{
At each date in the sequence (from start date to end date), for each PWS, the function queries the Wunderground historical database, and parses the xml.  This obtains weather conditions (temperature in C, dew point in C, humidity, wind speed in km/hr, wind gust in km/hr, pressure in mbar, precipitation rate in mm/hr, and precipitation total in mm) at various times (the times are local, not UTC).
}
\value{
The output is a list of PWS conditions, where each list element is a data frame of weather conditions for a particular PWS.
}
\references{
http://api.wunderground.com/
http://api.wunderground.com/api/b340be89948f3793/history_20140304/q/pws:KCASANFR231.xml
}
\author{
Ben Sheng
}

\examples{
startDate <- "2014-03-04"
endDate <- "2014-03-04"
Id <- c("KCASANFR231", "KCASANFR102", "KCASANFR53")
Lat <- c(37.78280, 37.79358, 37.77412)
Lon <- c(-122.4072, -122.3993, -122.3964)
PWS <- data.frame(Id, Lat, Lon)
PWSConditions <- getCond(startDate, endDate, PWS)
}
