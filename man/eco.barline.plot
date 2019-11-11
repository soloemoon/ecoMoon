\name{eco.barline.plot}
\alias{eco.barline.plot}
\alias{ggplot2}
\alias{plot}
\alias{visualization}
\title{Plot two series of economic data with built in recession bars}
\description{
Generates beautiful economic plots with built in recession bars}
\usage{
eco.barline.plot(df,x,bar,line,bar.color,line.color,title,caption,subtitle,line.size,date.break,date.format,y.title,x.title,legend.pos, xaxis.text.size,yaxis.text.size,title.size,caption.size,legend.size)
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

\value{Returns a plot with bars and lines}
\author{\email{soloemoon@gmail.com}}

\examples{
\dontshow{fred.api.key('a12c9684bc85a0d907b7f10bade21e93')}

fred.keys <-c('A191RL1Q225SBEA','A191RO1Q156NBEA')
key.rename <-c('GDPQ', 'GDPYOY')
gdp.data <-eco.quarterly(fred.keys,key.rename,'w')

eco.barline.plot(df=gdp.data,x='date',bar='GDPQ',line='GDPYOY',y.title='%',date.break='2 years',date.format='%y', legend.pos='top', bar.color ='blue',line.color ='#66CD00')

\keyword{aplot}

