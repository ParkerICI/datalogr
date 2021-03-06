% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datalog.R
\name{not}
\alias{not}
\title{Construct not clauses}
\usage{
not(...)
}
\arguments{
\item{...}{The individual clauses, constructed with \code{\link{d}}}
}
\value{
The expressions parsed in a way that is suitable
  for inclusion in a query
}
\description{
Construct not clauses
}
\examples{

qq <- query(find(?e),
    where(
        or(
           d(?e, gene/hugo),
           d(?e, variant/coordinates)
        )
    )
)


}
