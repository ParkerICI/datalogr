#' Creates pull expressions
#'
#' This function creates Datomic pull expressions. The following rules apply
#' when converting R expressions to Datomic pull expressions:
#' \itemize{
#'   \item Clojure vector literals (i.e. \code{[]}) are represented
#'     as \code{c()} in R
#'   \item Clojure map literalas (i.e. \code{{key val}}) are represented
#'     as \code{{key = val}} in R
#'   \item Back references are specified by pre-prending a \code{.} to the
#'     attribute name (instead of a \code{_}). E.g. the Clojure \code{variant/_gene}
#'     becomes \code{variant/.gene}
#'   \item The wildcard pattern \code{*} is represented as \code{.} in R
#'   \item Strings containing the \code{/} character are assumed to represent
#'     qualified symbol names and are converted by pre-prending the \code{:} character.
#'     E.g. the Clojure \code{:variant/gene} becomes \code{variant/gene}
#' }
#'
#' @param ... This function takes exactly 2 arguments which must be specified in
#'   the following order (named arguments are not supported, the arguments
#'   are matched positionally)
#' \itemize{
#'   \item The variable representing the entity id from which the pull is initiated
#'   \item The pull expression
#' }
#' @return Returns a data structure representing the pull expression, suitable
#'   for inclusion in the \code{find} clause of a query
#'
#' @examples
#' qq <- query(
#'   find(pull(?s, c({sample/subject = c(.)}, .))),
#'   where(
#'     d(?s, sample/id, ?id)
#'   )
#' )
#' do_query(qq)
#'
#' qq <- query(
#'   find(pull(?g, c(gene/hugo, {variant/.gene = c({variant/type = c(db/ident)}, .)})))
#'   where(
#'     d(?g, gene/hugo, ?hugo)
#'   )
#' )
#' do_query(qq)
#'
#' @export
pull <- function(...) {
    exprs <- rlang::exprs(...)
    if(length(exprs) != 2)
        stop("This function takes exactly 2 arguments")

    v <- process_question_mark(deparse(exprs[[1]]))

    ret <- parse_pull_data_literals(exprs[[2]])

    ret <- stringr::str_replace_all(ret, " ", "")
    ret <- paste(ret, collapse = "")
    ret <- stringr::str_replace_all(ret, "([\\w-]+/\\.?[\\w-]+)", "\":\\1\"")
    ret <- stringr::str_replace_all(ret, "(\\.)(\\w+)", "_\\2")
    ret <- stringr::str_replace_all(ret, "\\.", "\"*\"")
    ret <- stringr::str_replace_all(ret, "\"\"", "\"") # Remove double quotes
    ret <- stringr::str_replace_all(ret, "(\\bas\\b)(\\W+)(\\w+)", "\":as\", \"\\3\"") # Support for :as
    ret <- eval(parse(text = ret))
    ret <- list("pull", v, ret)
    return(ret)
}



#' Parses data literals in a pull expression
#'
#' This function parses the data literals in a pull expression
#' (\code{c()} and \code{{key = val}})
#' and converts them to the appropriate R function calls
#'
#' @param s An unevaluated R expression to be deparsed and processed
#' @return A string representation of the expression, with the appropriate
#'   substitutions performed
#'
parse_pull_data_literals <- function(s) {
    ret <- deparse(s)
    ret <- gsub("c\\(|\\{", "list(", ret)
    ret <- gsub("\\}", ")", ret)

    return(ret)
}


function() {
    x <- pull("foo", c(gene/hugo, {variant/.gene = c({variant/type = c(db/ident, as, ident)}, .)}))
    jsonlite::toJSON(x, auto_unbox = T)

    qq <- query(
        find(?v, pull(?g, c(gene/hugo, {variant/.gene = c({variant/type = c(db/ident, as, ident)}, .)}))),
        where(
            d(?g, gene/hugo, ?hugo),
            d(?v, variant/gene, ?g)
        ),
        args(?hugo <- "CDKN3")

    )

    qq <- query(
        find(?s, pull("?s", c(.))),
        where(
            d(?s, sample/id, ?id)
        )

    )

    qq <- query(
        find(?id, pull("?s", c({sample/subject = c(.)}, .))),
        where(
            d(?s, sample/id, ?id)
        )

    )
}

