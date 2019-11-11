\name{eco.bar.plot}
\alias{eco.bar.plot}
\alias{ggplot2}
\alias{plot}
\alias{visualization}
\title{Plot barplot of economic series}
\description{
Generates beautiful economic plots with built in recession bars}
\usage{
eco.bar.plot(df,x,y,fill,yTitle,title,caption,labels,subtitle, legend.pos,bar.position,xaxis.text.size,yaxis.text.size,title.size,caption.size,legend.size)
}
\arguments{
  \item{df}{Dataframe containing the data to be plotted}
  \item{x}{Name of column to be used as x axis. This should be a string.}
  \item{bar}{Name of column containing bar data to be plotted. This should be a string.}
  \item{line}{Name of column used to plot line on y axis. This should be a string.}
  \item{bar.color}{Set color of bars.}
  \item{line.color}{Set color of line.}
  \item{subtitle}{Subtitle Text}
  \item{y.title}{Y Axis Title}
  \item{x.title}{X Axis Title}
  \item{caption}{Caption Text}
  \item{date.break}{X Axis date seperator. Should be string. Ex. '2 years'}
  \item{color}{Color of Y Axis line}
  \item{date.format}{Month Numeric: '%m', Month Abbr: '%b', Month Full: '%B', Year   Abbr: '%y', Year Full: '%Y'. Default is '%y'}
  \item{line.size}{Set size of line. Default is .7}
  \item{yaxis.text.size}{Set size of y Axis text. Default is 15}
  \item{xaxis.text.size}{Set size of x Axis text. Default is 15}
  \item{title.size}{Set size of title. Default is 20}
  \item{caption.size}{Set size of caption. Default is 13}
  \item{legend.size}{Set size of legend. Default is 15}
  \item{legend.pos}{Set location of the legend. This is a string. Default is 'top'}
}

\value{Returns a bar plot}
\author{\email{soloemoon@gmail.com}}

\examples{
\dontshow{fred.api.key('a12c9684bc85a0d907b7f10bade21e93')}

fred.keys <-c('DPCERY2Q224SBEA','A006RY2Q224SBEA','A822RY2Q224SBEA','A019RY2Q224SBEA')
key.rename <-c('Consumption','Investment','Government','Net Exports')

gdp.data <-eco.quarterly(fred.keys,key.rename)

eco.bar.plot(df=gdp.data, x='Quarter',y='series_id',fill='series_id')

\keyword{aplot}

