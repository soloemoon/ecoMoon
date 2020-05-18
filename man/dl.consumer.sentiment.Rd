\name{dl.consumer.sentiment}
\alias{dl.consumer.sentiment}
\alias{ggplot2}
\alias{plot}
\alias{visualization}
\title{Download Consumer Sentiment}
\description{
Downloads Consumer Sentiment data from the University of Michigan}
\usage{
dl.consumer.sentiment(save.location, start.date)
}
\arguments{
  \item{save.location}{Location to save data set too}
  \item{start.date}{Date to subset by}

}

\value{Returns Consumer Sentiment Data}
\author{\email{soloemoon@gmail.com}}

\examples{
\dontshow{fred.api.key('a12c9684bc85a0d907b7f10bade21e93')}

consumer.sentiment <-dl.consumer.sentiment(file.link, '2000-01-12')
\keyword{aplot}

