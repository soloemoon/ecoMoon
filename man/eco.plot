\name{eco.plot}
\alias{eco.plot}
\alias{ggplot2}
\alias{plot}
\alias{visualization}
\title{Plot a single series of economic data with built in recession bars}
\description{
Generates beautiful economic plots with built in recession bars}
\usage{
eco.plot(df,x,y,sub,y.title,x.title,title,caption,date.break,color,date.format,line.size, yaxis.text.size,xaxis.text.size,title.size,caption.size,legend.size,legend.pos)
}
\arguments{
  \item{df}{Dataframe containing the data to be plotted}
  \item{x}{Name of column to be used as x axis. This should be a string.}
  \item{y}{Name of column to be used as y axis. This should be a string.}
  \item{sub}{Subtitle Text}
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

\value{Returns a single series plot}
\author{\email{soloemoon@gmail.com}}

\examples{
\dontshow{fred.api.key('a12c9684bc85a0d907b7f10bade21e93')}
fred.keys <-c('UNRATE','PAYEMS')
key.rename <-c('Unemployment Rate','Non Farm Payrolls')
monthly.indicators <-eco.monthly(fred.keys, key.rename,'w')

eco.plot(df=monthly.indicators, x='date',y='Unemployment Rate',subtitle = 'sample plot',y.title='%',x.title='date',date.break='2 years',date.format='%y',title.size=22,legend.pos='top')

\keyword{aplot}
