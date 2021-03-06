% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datalog.R
\name{process_question_mark}
\alias{process_question_mark}
\title{Process R expressions starting with "?"}
\usage{
process_question_mark(s)
}
\arguments{
\item{s}{A string representing the R expression to be converted}
}
\value{
Returns the converted string
}
\description{
This function converts R expressions containing variables starting with \code{?}. Such expressions
look like symbol names but are actually converted to function calls
when parsed by the R interpreter
}
