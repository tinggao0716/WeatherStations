\name{loadDf}
\alias{loadDf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
TBD
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
loadDf(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
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
          filename <- "test.rds"
          saveDf(ws, file=filename)
          ws_loaded <- loadDf(filename)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
