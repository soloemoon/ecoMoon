\name{eco.plot2}
\alias{eco.plot2}
\alias{ggplot2}
\alias{plot}
\alias{visualization}
\title{Plot two series of economic data with built in recession bars}
\description{
Generates beautiful economic plots with built in recession bars}
\usage{
eco.plot2(df,x,y1,y2,ycolor1,ycolor2,subtitle,y.title,x.title,title,caption,legend.pos,date.break, date.format,line.size,yaxis.text.size,xaxis.text.size,title.size,caption.size,legend.size)
}
\arguments{
  \item{df}{Dataframe containing the data to be plotted}
  \item{x}{Name of column to be used as x axis. This should be a string.}
  \item{y1}{Name of 1st column to be used on y axis. This should be a string.}
  \item{y2}{Name of 2nd column to be used on y axis. This should be a string.}
  \item{ycolor1}{Set color of first line.}
  \item{ycolor2}{Set color of second line.}
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

\value{Returns a dual series plot}
\author{\email{soloemoon@gmail.com}}

\examples{
\dontshow{fred.api.key('a12c9684bc85a0d907b7f10bade21e93')}
fred.keys <-c('UNRATE','(LNS14000024')
key.rename <-c('Unemployment Rate','20yr Unemployment')
monthly.indicators <-eco.monthly(fred.keys, key.rename,'w')

eco.plot2(df=monthly.indicators,x='date',y1='Unemployment Rate',y2='20yr Unemployment',y.title='%',date.break='2 years',date.format='%y', legend.pos='top')

\keyword{aplot}

