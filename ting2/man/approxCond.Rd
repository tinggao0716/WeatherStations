\name{approxCond}
\alias{approxCond}

\title{
approximate weather conditions
}
\description{
Uses linear approximation to return the weather conditions for a specified time. 
}
\usage{
approxCond(inputTime, stationdata, weatherCond)
}

\arguments{
  \item{inputTime}{
the input time (in the format "YYYY-MM-DD hh:mm")
}
  \item{stationdata}{
a data frame of regional PWS
}
  \item{weatherCond}{
a list of PWS conditions (this is the output from the getCond function)
}
}

\details{
For each PWS and weather condition, the function uses linear approximation to estimate the value at the input time.  The function also handles missing data (some PWS have no data for a particular date) and invalid data (e.g., negative wind speed).
}
\value{
The output is a data frame of weather conditions estimated at the input time for each PWS.
}
\references{
http://api.wunderground.com/
http://api.wunderground.com/api/b340be89948f3793/history_20140304/q/pws:KCASANFR231.xml
}
\author{
Ben Sheng
}

\examples{
  1+1
}

