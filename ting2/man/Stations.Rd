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
lectures}
\author{  Ting Gao}


%\examples{
%library(XML)
%library(ggplot2)
%library(gridExtra)
%library(RgoogleMaps)
%library(ggmap)

%location <- c(37.786289,-122.405234)
%dist_km <- 30
%# This call needs web access
%df <- Stations(location, dist_km)
%PwsStations <- WeatherStation(df)
%}


