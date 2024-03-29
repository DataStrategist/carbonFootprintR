% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcs.R
\name{exchangerateGetter}
\alias{exchangerateGetter}
\title{exchangerateGetter}
\usage{
exchangerateGetter(desiredcurrency = "USD")
}
\arguments{
\item{desiredcurrency}{what currency would we like to convert to, Default: USD}
}
\value{
returns the numeric number of SEKs to the desired currency
}
\description{
get today's exchange rate SEK to USD
}
\details{
The API gives SEK by default, we want the output to do USD instead
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
