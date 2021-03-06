% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{do_query}
\alias{do_query}
\title{Runs a query agains the database}
\usage{
do_query(
  query,
  server.url = NULL,
  timeout = NULL,
  print.json = FALSE,
  auth.token = NULL,
  verbose = FALSE,
  optimize = TRUE,
  ...
)
}
\arguments{
\item{query}{The query as returned by \code{\link{query}}}

\item{server.url}{Optional. The server URL against which the
query is run. This can be set globally using \code{\link{set_server_url}}}

\item{timeout}{Optional. The query timeout in milliseconds}

\item{print.json}{If set to \code{TRUE}, prints the JSON version of the query
that is sent to the server (useful for debugging purposes)}

\item{auth.token}{Optional string. The authorization token to be used for this query}

\item{verbose}{Optional, currently unused}

\item{optimize}{Whether to use the server-side query optimizer}

\item{...}{Additional arguments passed to either \code{\link{convert_query_results}}
or either \code{\link{convert_pull_query_results}}}
}
\value{
Returns a \code{data.frame} or vector with the query results. In the former
  case the column names are taken from the symbols that appear in the \code{find}
  portion of the query
}
\description{
Runs a query agains the database
}
