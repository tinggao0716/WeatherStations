\name{plotCond}
\alias{plotCond}

\title{
plot weather conditions
}
\description{
Plots a particular weather condition for a given set of weather stations, and prints the plot to pdf.
}
\usage{
plotCond(StationCond, condition)
}

\arguments{
  \item{StationCond}{
data frame of weather conditions for each PWS (this is the output from the approxCond function)
}
  \item{condition}{
the variable name for a weather condition (e.g., "temperatureC")
}
}

\details{
The function uses RgoogleMaps, ggmap, and ggplot in order to plot the PWS against a Google maps background.  In the plot, the weather condition magnitude is shown in shades of blue (higher values are shown in lighter blue, and lower values are shown in darker blue); invalid data points are represented as gray dots.
}
\value{
The output is a pdf, and the pdf name is the variable name.
}
\references{
http://api.wunderground.com/
http://api.wunderground.com/api/b340be89948f3793/history_20140304/q/pws:KCASANFR231.xml
http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
}
\author{
Ben Sheng
}
\note{
Examine the packages RgoogleMaps and ggmap.
}

\examples{
}
