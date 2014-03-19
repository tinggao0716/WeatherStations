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
S4 class for stations table
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
SoDA
}
\author{
Ting Gao
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
showClass("WeatherStation")
}
\keyword{classes}
