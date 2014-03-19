\name{Stations}
\alias{Stations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Function to extract all PWS given a location and distance.
}
\description{
Connect to wunderground website, and extract the XML given location and distance. Then parse the XML and put the data in a data frame.
}
\usage{
Stations(location, dist_km)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{location}{
%%     ~~Describe \code{location} here~~
}
  \item{dist_km}{
%%     ~~Describe \code{dist_km} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  Ting Gao}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(XML)
library(ggplot2)
library(gridExtra)
library(RgoogleMaps)
library(ggmap)

location <- c(37.786289,-122.405234)
dist_km <- 30
# This call needs web access
df <- Stations(location, dist_km)
PwsStations <- WeatherStation(df)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
