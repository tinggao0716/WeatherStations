\name{plot.WeatherStation}
\alias{plot.WeatherStation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Function to plot locations of weather stations
}
\description{
plot station locations}
\usage{
plot.WeatherStation(x, ...)
}
%- maybe also 'usage' for other objects documented here.
%\arguments{
%  \item{x}{
%%     ~~Describe \code{x} here~~
%}
%  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
%}
%}
%\details{}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
SoDa}
\author{
Ting Gao
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
library(XML)
library(ggplot2)
library(gridExtra)
library(RgoogleMaps)
library(ggmap)

createTestDf <- function(){
  data.frame(
             City= c("A", "BC"),
             State=c("CA", "IL"),
             Country=c("Santa Clara", "Evanston"),
             Id=c("af4", "safsAS"),
             Lat=c(12.3, 2.241),
             Lon=c(34.5, 8.1),
             Distance=c(2,5),
             stringsAsFactors=FALSE)
}
  df <- createTestDf()
  ws<-WeatherStation(df)
  plot(ws)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
