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
#' @param auth.token Optional string. The authorization token to be used for this query
#' @param verbose Optional, currently unused
#' @param optimize Whether to use the server-side query optimizer
#' @param ... Additional arguments passed to either \code{\link{convert_query_results}}
#'   or either \code{\link{convert_pull_query_results}}
#' @return Returns a \code{data.frame} or vector with the query results. In the former
#'   case the column names are taken from the symbols that appear in the \code{find}
#'   portion of the query
#' @export
do_query <- function(query, server.url = NULL, timeout = NULL, print.json = FALSE, auth.token = NULL, verbose = FALSE, optimize = TRUE, ...) {
    server.url <- ensure_server_url(server.url)
    qq <- query
    if(is.null(qq$query$"in"))
        qq$query$"in" <- list("$")
    else
        qq$query$"in" <- c("$", qq$query$"in")
    qq$timeout <- timeout
    qq$async <- TRUE # To support old API, remove eventually
    qq$optimize <- optimize

    pull.query <- is_pull_query(query)

    names(qq$query) <- paste(":", names(qq$query), sep = "")

    query.json <- jsonlite::toJSON(qq, auto_unbox = TRUE)

    if(print.json)
        message(query.json)

    headers <- httr::add_headers(Authorization = paste("Token", auth.token))
    response <- httr::RETRY("POST", url = server.url, body = query.json, encode = "raw", config = headers)

    response.content <- httr::content(response, simplifyVector = TRUE)

    ret <- NULL

    if (httr::status_code(response) == 200) {
        res.data <- res.data.content <- NULL

        if(is.list(response.content) && !is.null(response.content$"query-id")) {
            query.id <- response.content$"query-id"
            server.base.url <- gsub("/query/.*", "", server.url)
            while(is.null(res.data)) {
                x <- httr::GET(url = paste(server.base.url, "query-status", query.id, sep = "/"), config = headers)

                if(httr::status_code(x) == 200) {
                    x <- httr::content(x, simplifyVector = TRUE)
                    if(x$status %in% c("fail-query", "fail-malformed-query", "fail-error", "fail-memory")) {
                        if ("error-message" %in% colnames(x)) {
                            candel.error <- x$`error-message`
                        } else {
                            candel.error <- "Server error message was empty!"
                        }
                        stop(sprintf("Query failed with status: %s\nerror message: %s", x$status, candel.error))
                    }
                    else if(x$status %in% c("success", "success-cached")) {
                        res.data <- httr::RETRY("GET", url = x$"results-url", times = 1000, quiet = T)
                        res.data.content <- httr::content(res.data, simplifyVector = TRUE)
                    }
                } else {
                    warning(sprintf("Failed to retrieve query status for query-id:%s", query.id))
                }
                Sys.sleep(1)
            }

        } else { # Support for old query server, remove eventually
            if (is.character(response.content) && substr(response.content, 1, 8) == 'https://') {
                res.data <- httr::RETRY("GET", url = response.content, times = 1000, quiet = T)
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
            if(!is.null(res.data.content$error))
                stop(sprintf("Error %s", res.data.content$error$message))
            res.data.content <- res.data.content$query_result

        }

        if (pull.query)
            ret <- convert_pull_query_results(res.data.content, ...)
        else
            ret <- convert_query_results(query, res.data.content, ...)
    } else if (httr::status_code(response) == 401) {
        stop("Not authorized. Either you didn't provide an authorization token, or your token has expired")
    } else {
        if(!is.null(response$message))
            stop(response$message)
        else
            stop(response.content)
    }

    return(ret)
}


#' Checks whether a query contains a pull expression
#'
#' @param query
#' @return Returns a boolean indicating whether this query contains a pull expression
#'   or not
#'
#' @export
#'
is_pull_query <- function(query) {
    any(grepl("pull", unlist(query$query$find)))
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

