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
  \item{transformation}{Transformation to apply (Required)}
  \item{shape}{Choose 'l' for long format or 'W' for wide format. Default is wide.}
  \item{periodicity}{Choose periodicity. Either 'monthly', 'quarterly', 'weekly', or 'daily'. Default is monthly.}
  \item{show.recession}{Choose y to include recession indicator or 'n' to exclude. Default is 'y'.}

}

\value{Returns a preformatted dataframe with the given economic data.}
\references{
\url{https://research.stlouisfed.org/docs/api/fred/}
}
\author{\email{soloemoon@gmail.com}}

\examples{
\dontshow{fred.api.key('a12c9684bc85a0d907b7f10bade21e93')}
fred.keys <-c('UNRATE','PAYEMS')
key.rename <-c('Unemployment Rate','Non Farm Payrolls')
monthly.indicators <-eco.monthly(fred.keys, key.rename,shape = 'w', periodicity = 'monthly', show.recession = 'y')

\keyword{datasets}
