% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datalog.R
\name{not_join}
\alias{not_join}
\title{Construct not-join clauses}
\usage{
not_join(vars, ...)
}
\arguments{
\item{vars}{The variables that should unify with the surronding clauses, constructed
with \code{\link{d}}}

\item{...}{The clauses constructed with \code{\link{d}}}
}
\value{
The expressions parsed in a way that is suitable
  for inclusion in a query
}
\description{
Construct not-join clauses
}
\examples{
qq <- query(find(count(?artist)),
            where(
                d(?artist, artist/name),
                not_join(d(?artist),
                         d(?release, release/artists, ?artist),
                         d(?release, release/year, 1970)
                )
            ))

}
