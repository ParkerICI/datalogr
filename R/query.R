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
#' @param query.timeout Optional. The query timeout in milliseconds
#' @param datalogr.timeout Optional. Time for datalogr to wait for query to finish in milliseconds
#' @param print.json If set to \code{TRUE}, prints the JSON version of the query
#'   that is sent to the server (useful for debugging purposes)
#' @param ... Additional arguments passed to either \code{\link{convert_query_results}}
#'   or either \code{\link{convert_pull_query_results}}
#' @return Returns a \code{data.frame} or vector with the query results. In the former
#'   case the column names are taken from the symbols that appear in the \code{find}
#'   portion of the query
#' @export
do_query <- function(query, server.url = NULL, query.timeout = NULL, datalogr.timeout = NULL, print.json = FALSE, ...) {
    server.url <- ensure_server_url(server.url)
    qq <- query
    if(is.null(qq$query$"in"))
        qq$query$"in" <- list("$")
    else
        qq$query$"in" <- c("$", qq$query$"in")
    if(!is.null(query.timeout)){
        qq$timeout <- query.timeout
        qq$async <- TRUE
    }
    qq$optimize <- TRUE

    pull.query <- any(grepl("pull", unlist(query$query$find)))

    names(qq$query) <- paste(":", names(qq$query), sep = "")

    query.json <- jsonlite::toJSON(qq, auto_unbox = TRUE)

    if(print.json)
        message(query.json)

    response <- httr::RETRY("POST", url = server.url, body = query.json, encode = "raw")

    response.content <- httr::content(response, simplifyVector = TRUE)


    ret <- NULL

    if (httr::status_code(response) == 200) {
        res.data.content <- NULL

        if (is.character(response.content) && substr(response.content, 1, 8) == 'https://') {
            if(is.null(datalogr.timeout)){
                res.data <- httr::RETRY("GET", url = response.content)
            } else {
                # The s3 link to results will return a 403 if the query is not completed yet.
                res.code <- 403
                datalogr.timeout.seconds <- datalogr.timeout %/% 1000
                R.utils::withTimeout(
                    while(res.code == 403){
                        res.data <- httr::GET(response.content)
                        res.code <- httr::status_code(res.data)
                        if(res.code == 403){
                            Sys.sleep(time = 5)
                            message("Query not completed. Trying again.")
                        }
                    }, timeout = datalogr.timeout.seconds, onTimeout = "warning"
                )
            }

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

