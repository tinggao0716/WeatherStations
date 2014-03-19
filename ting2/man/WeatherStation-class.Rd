\name{WeatherStation-class}
\Rdversion{1.1}
\docType{class}
\alias{WeatherStation-class}
\alias{getDf,WeatherStation-method}
\alias{plot,WeatherStation,missing-method}
\alias{saveDf,WeatherStation-method}
\alias{subset,WeatherStation-method}

\title{Class \code{"WeatherStation"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("WeatherStation", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{stationdata}:}{Object of class \code{"data.frame"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{getDf}{\code{signature(x = "WeatherStation")}: ... }
    \item{plot}{\code{signature(x = "WeatherStation", y = "missing")}: ... }
    \item{saveDf}{\code{signature(x = "WeatherStation")}: ... }
    \item{subset}{\code{signature(x = "WeatherStation")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("WeatherStation")
}
\keyword{classes}
