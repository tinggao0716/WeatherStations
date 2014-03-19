\name{WeatherStation}
\alias{WeatherStation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
S4 class for storing weather stations' information
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WeatherStation(stationdata = data.frame(City = character(), State = character(), Country = character(), Id = character(), Lat = numeric(), Lon = numeric(), Distance = numeric()))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stationdata}{
%%     ~~Describe \code{stationdata} here~~
}
}

\references{
SoDa
}
\author{
Ting Gao}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
df <-
  data.frame(
             City= c("A", "BC"),
             State=c("CA", "IL"),
             Country=c("Santa Clara", "Evanston"),
             Id=c("af4", "safsAS"),
             Lat=c(12.3, 2.241),
             Lon=c(34.5, 8.1),
             Distance=c(2,5),
             stringsAsFactors=FALSE)

ws<-WeatherStation(df)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ class}

