\name{eco.download}
\alias{eco.download}
\alias{economic}
\alias{fred}
\alias{bloomberg}

\title{Download Economic Data}
\description{
Download economic data from selected sources. Choose shape, inclusion of recession bars, and time series options.
\usage{
eco.monthly(keys,key.rename, shape, periodicity, show.recession)
}
\arguments{
  \item{fred.keys}{String of Fred key names to import.}
  \item{key.rename}{String of new user defined names for keys.}
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
keys <-c('UNRATE','PAYEMS')
rename <-c('Unemployment Rate','Non Farm Payrolls')
t.data <-eco.download(c('UNRAte','PAYEMS'), c('UE','Payrolls'), show.recession = 'n', time.series = 'n')

\keyword{datasets}
