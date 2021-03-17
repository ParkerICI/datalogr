#' Process R expressions starting with "?"
#'
#' This function converts R expressions containing variables starting with \code{?}. Such expressions
#' look like symbol names but are actually converted to function calls
#' when parsed by the R interpreter
#'
#' @param s A string representing the R expression to be converted
#' @return Returns the converted string
#'
#'
process_question_mark <- function(s) {
    ret <- gsub("`", "", s)
    if(length(grep("\\?", ret)) > 0) {
        regex <- "\\?\\(([^\\(\\)]*)\\)"

        m <- stringr::str_extract_all(ret, regex)
        s <- sapply(m[[1]], function(x){
            sprintf("?%s", stringr::str_match(x,regex)[,2])})
        s <- gsub(" - ", "-", s)
        # Fix named vector names to be regex safe:
        names(s) <- sapply(names(s), function(x){stringr::str_replace_all(x, "(\\W)", "\\\\\\1")})
        ret <- stringr::str_replace_all(ret, s)

    }
    ret <- trimws(ret)
    return(ret)
}

#maybe this should be renamed to parse_sumbol
parse_expression <- function(s) {
    ret <- s

    if(!is.character(ret)) {
        ret <- deparse(ret)
        ret <- process_question_mark(ret)

        #It's a keyword
        if(length(grep("[a-z]/[a-z]", ret, ignore.case = T)) > 0) {
            ret <- gsub(" ", "", ret)
            ret <- paste(":", ret, sep = "")
        }

        #Infix operator
        if(length(grep("<|>", ret)) > 0) {
            op <- regmatches(ret, gregexpr("<|>", ret))[[1]]
            v <- strsplit(ret, "<|>")[[1]]
            ret <- list(op, v[[1]], v[[2]])
        }

        #Vector: processing vectors written as c(?a, ?b) to list(?a, ?b)
        if(startsWith(ret, "c(")){
            m <- gsub(" ", "", ret)
            m <- stringr::str_match(m, "c(.*)\\((.*)\\)")
            ret <- as.list(strsplit(m[3], ",")[[1]])

        }

        #Function call
        if(length(grep("(.*)\\((.*)\\)", ret)) > 0) {
            m <- stringr::str_match(ret, "(.*)\\((.*)\\)")
            ret <- list(m[1, 2], m[1, 3])
        }

        #Single dot
        if(length(ret) == 1 && ret == ".")
            ret <- "_"

    }
    if(is.list(ret))
        ret <- lapply(ret, try_numeric)
    else
        ret <- try_numeric(ret)

    return(ret)

}

#' Attempt conversion to numeric
#'
#' @param s The string to convert
#' @return Returns \code{as.numeric(s)}, os \code{s} if numeric conversion fails
#'
try_numeric <- function(s) {
    tryCatch(as.numeric(s),
             warning = function(cond) {return(trimws(s))})
}


#' Convert expressions to Datalog
#'
#' This function converts R expressions to Datalog. The following rules apply
#' when converting expressions to Datalog:
#' \itemize{
#'   \item Infix operators are converted to prefix
#'   \item Function calls are converted to S-expressions (e.g. max(v) is converted to (max v))
#'   \item The \code{.} symbol is converted to \code{_}
#'   \item String containing the \code{/} character are assumed to represent
#'     qualified symbol names and are converted by pre-prending the \code{:} character
#'   \item Names prefixed with \code{!!} are assumed to represent R symbols and are substituted
#'     with their bound value
#' }
#' Note the following:
#' \itemize{
#'   \item This function only performs syntax conversion and does not check the
#'     correctness of the resulting expressions
#'   \item For the same reason, even though you may use function names that exist in R
#'     (e.g. \code{max}), this function is not actually invoking the corresponding R
#'     function at all, therefore you should not assume that you can pass it the same arguments
#'     that you would in R. For instance the expression \code{max(v, is.na = TRUE)} would make no sense
#'     since the R \code{max} function is not invoked at any point during the conversion process
#' }
#'
#' @param ... The epxressions to convert to Datalog
#' @return Returns a list of strings where each element
#'   corresponds to the string representation of the corresponding expression
#'   in \code{...}
#'
#' @export
#'
d <- function(...) {
    ret <- rlang::exprs(...)
    ret <- lapply(ret, parse_expression)
    names(ret) <- NULL
    return(ret)
}

#' Construct the where portion of a Datalog query
#'
#' @param ... R expressions to be converted using \code{\link{d}}
#' @return A list of parsed expressions, suitable for inlcusion in a
#'   \code{\link{query}}
#' @export
where <- function(...) {
    ret <- rlang::exprs(...)
    ret <- lapply(ret, eval)
    names(ret) <- NULL

    return(list(where = ret))
}


#' Construct the find portion of a Datalog query
#'
#' @param ... R expressions to be converted using \code{\link{d}}
#' @return A list of parsed expressions, suitable for inlcusion in a
#'   \code{\link{query}}
#' @export
find <- function(...) {
    exprs <- rlang::exprs(...)
    ret <- lapply(exprs, function(e) {
        if(startsWith(deparse(e)[[1]], "pull"))
            eval(e)
        else
            unlist(d(!!e))

    })
    names(ret) <- NULL

    return(list(find = ret))

}


#' Construct a Datalog query
#'
#' Constructus a Datalog query
#'
#' @param ... The components of the query, these should be invocations
#'   of the \code{\link{find}}, \code{\link{where}} and optionally \code{\link{args}}
#'   functions
#' @return Returns the Datalog query in list representation
#'
#' @export
query <- function(...) {
    ll <- unlist(list(...), recursive = FALSE)
    args <- ll$args
    ll$args <- NULL
    ret <- list(query = ll)
    ret$args <- args

    return(ret)
}



#' Parse an individual query argument
#'
#' @param s The expression to parse
#' @return Returns the expression parsed into a named list with two elements
#'   named \code{in} and \code{args}
#'
parse_arg <- function(s) {
    ret <- deparse(s)
    ret <- process_question_mark(ret)
    ret <- strsplit(ret, "<-")[[1]]
    ret <- trimws(ret)
    ret <- gsub("\"", "", ret)
    ret[2] <- try_numeric(ret[2])

    return(setNames(list(ret[1], ret[2]), c("in", "args")))

}

#' Construct query arguments
#'
#' This function constructs arguments for a query starting
#' from a series of R expressions
#'
#' @param ... The R epxressions that represents the arguments to the query.
#'   Each expression must be of the form \code{variable <- value},
#'   which will cause \code{value} to be bound to \code{variable} in the query
#'
#'
#' @return A list of the epxressions parsed in a way that is suitable
#'   for inclusion in a query.
#'
#' @export
args <- function(...) {
    ret <- rlang::exprs(...)
    ret <- lapply(ret, parse_arg)

    ret <- list(as.list(as.vector(sapply(ret, "[[", "in"))),
                as.list(as.vector(sapply(ret, "[[", "args"))))

    names(ret) <- c("in", "args")
    return(ret)

}


#' Construct or clauses
#'
#' @param ... The individual clauses, constructed with \code{\link{d}}
#'
#' @return The expressions parsed in a way that is suitable
#'   for inclusion in a query
#'
#' @examples
#'
#' qq <- query(find(?e),
#'     where(
#'         or(
#'            d(?e, gene/hugo),
#'            d(?e, variant/coordinates)
#'         )
#'     )
#' )
#'
#'
#' @export
or <- function(...) {
    exprs <- rlang::exprs(...)

    clauses <- lapply(exprs, eval)
    names(clauses) <- NULL
    ret <- do.call(list, c("or", clauses))

    return(ret)

}


#' Generate multiple or clauses for several alternatives
#'
#' This function generates multiple \code{or} clauses by combining a partial clause with multiple
#' alternatives. For instance given the alternatives \code{c("A", "B")} and the partial clause
#' \code{?p, epitope/name} this function generates the or clause
#' \code{
#'   or(
#'     d(?p, epitope/name, "A"),
#'     d(?p, epitope/name, "B")
#'   )
#' }
#'
#' @param alternatives A character vector of alternatives
#' @param ... The partial clause
#'
#' @examples
#'
#' generate_or(
#'   alternatives = c("A", "B"), ?p, epitope/name
#' )
#'
#' @return Returns an \code{or} expression suitable for inclusion in a query
#' @export
generate_or <- function(alternatives, ...) {
    ret <- lapply(alternatives, function(x) {d(..., !!x)})
    ret <- do.call(list, c("or", ret))
    return(ret)
}


#' Construct not clauses
#'
#' @inherit or
#' @export
not <- function(...) {
    exprs <- rlang::exprs(...)

    clauses <- lapply(exprs, eval)
    names(clauses) <- NULL
    ret <- do.call(list, c("not", clauses))

    return(ret)

}


#' Construct not-join clauses
#'
#' @param vars The variables that should unify with the surronding clauses, constructed
#'   with \code{\link{d}}
#' @param ... The clauses constructed with \code{\link{d}}
#'
#' @return The expressions parsed in a way that is suitable
#'   for inclusion in a query
#'
#' @examples
#' qq <- query(find(count(?artist)),
#'             where(
#'                 d(?artist, artist/name),
#'                 not_join(d(?artist),
#'                          d(?release, release/artists, ?artist),
#'                          d(?release, release/year, 1970)
#'                 )
#'             ))
#'
#' @export
not_join <- function(vars, ...) {
    exprs <- rlang::exprs(...)

    clauses <- lapply(exprs, eval)
    names(clauses) <- NULL
    ret <- do.call(list, c("not-join", list(vars), clauses))

    return(ret)
}


#' Construct the with portion of a Datalog query
#'
#' @inherit find
#'
#' @export
with <- function(...) {
    ret <- d(...)
    return(list(with = ret))
}

#' Combine queries
#'
#' This function combines queries by concatenating their \code{find}, \code{where}
#' and \code{args} fields
#'
#' @param a A query as returned by \code{\link{query}}
#' @param b A query as returned by \code{\link{query}}
#'
#' @return Returns the fields of the \code{b} query appended to the corresponding fields
#'   of the \code{a} query
#'
#' @export
c_query <- function(a, b) {
    for(f in union(names(a$query), names(b$query))) {
        a$query[[f]] <- c(a$query[[f]], b$query[[f]])

    }

    a$args <- c(a$args, b$args)
    return(a)
}

