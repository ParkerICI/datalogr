pkg.env <- new.env()


#' Ensure server.url has been set correctly
#'
#' @param server.url The server URL, possibly \code{NULL}
#'
#' @return Returns \code{server.url} if not \code{NULL}. Otherwise returns the global
#'   \code{pkg.env$server.url} value
#'
#' @export
ensure_server_url <- function(server.url) {
    if(is.null(server.url)) {
        if(is.null(pkg.env$server.url))
            stop("Either provide a server.url or globally set the server URL using set_server_url")
        server.url <- pkg.env$server.url
    }
    return(server.url)
}


#' Runs a query agains the database
#'
#' @param query The query as returned by \code{\link{query}}
#' @param server.url Optional. The server URL against which the
#'   query is run. This can be set globally using \code{\link{set_server_url}}
#' @param timeout Optional. The query timeout in milliseconds
#' @param print.json If set to \code{TRUE}, prints the JSON version of the query
#'   that is sent to the server (useful for debugging purposes)
#' @param ... Additional arguments passed to either \code{\link{convert_query_results}}
#'   or either \code{\link{convert_pull_query_results}}
#' @return Returns a \code{data.frame} or vector with the query results. In the former
#'   case the column names are taken from the symbols that appear in the \code{find}
#'   portion of the query
#' @export
do_query <- function(query, server.url = NULL, timeout = NULL, print.json = FALSE, ...) {
    server.url <- ensure_server_url(server.url)
    qq <- query
    if(is.null(qq$query$"in"))
        qq$query$"in" <- list("$")
    else
        qq$query$"in" <- c("$", qq$query$"in")
    qq$timeout <- timeout

    pull.query <- any(grepl("pull", unlist(query$query$find)))

    names(qq$query) <- paste(":", names(qq$query), sep = "")

    query.json <- jsonlite::toJSON(qq, auto_unbox = TRUE)

    if(print.json)
        message(query.json)

    response <- httr::RETRY("POST", url = server.url, body = query.json, encode = "raw", async = TRUE)

    response.content <- httr::content(response, simplifyVector = TRUE)


    ret <- NULL

    if (httr::status_code(response) == 200) {
        res.data.content <- NULL

        if (is.character(response.content) && substr(response.content, 1, 8) == 'https://') {
            res.data <- httr::RETRY("GET", url = response.content)
            res.data.content <- httr::content(res.data, simplifyVector = TRUE)

            if(httr::status_code(res.data) != 200) {
                if(!is.null(res.data$message))
                    stop(res.data$message)
                else
                    stop(res.data.content)
            }
        }
        else
            res.data.content <- response.content

        if (pull.query)
            ret <- convert_pull_query_results(res.data.content$query_result, ...)
        else
            ret <- convert_query_results(query, res.data.content$query_result, ...)

    } else {
        if(!is.null(response$message))
            stop(response$message)
        else
            stop(response.content)
    }

    return(ret)
}




#' Globally set the server URL
#'
#' This function sets the server URL globally for the entire package (so that you
#'   don't have to pass around a \code{server.url} argument with every query)
#'
#' @param server.url The server URL
#' @return Returns the previous \code{server.url} value, invisibly
#' @export
set_server_url <- function(server.url) {
    old <- pkg.env$server.url
    pkg.env$server.url <- server.url
    return(invisible(old))
}

