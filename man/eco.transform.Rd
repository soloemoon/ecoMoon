\name{eco.transform}
\alias{eco.transform}
\alias{economic}
\alias{fred}
\alias{bloomberg}

\title{Apply common economic transformations to entire dataset}
\description{
Transform economic dataset with a selected number of available transformations
}
\usage{
eco.transform(df, transformation, lags, index.date)
}
\arguments{
  \item{df}{Dataframe containing economic data (Required)}
  \item{transformation}{Transformation to apply: '% change', 'change','log','annualize monthly','annualize quarterly'}
  \item{lags}{Select the number of lags to apply}
  \item{index.data}{Input a date for the index transformation}

}

\value{Returns a preformatted dataframe with the given economic data.}
\references{
\url{https://research.stlouisfed.org/docs/api/fred/}
}
\author{\email{soloemoon@gmail.com}}

\examples{
\dontshow{fred.api.key('a12c9684bc85a0d907b7f10bade21e93')}
t.data <-eco.download(c('UNRAte','PAYEMS'), c('UE','Payrolls'), show.recession = 'n', time.series = 'n')
t <-eco.transform(df = t.data, transformation = '% change', lags = 1)
\keyword{datasets}
