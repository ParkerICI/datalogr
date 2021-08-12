

#' Flattens a list of data frames
#'
#' This function flattens a list of data frames
#' using \code{jsonlite::flatten}
#'
#' @param x A list of data frames
#' @return Returns a \code{data.fame}
#'
flatten_data_frame <- function(x) {
    res <- lapply(x, jsonlite::flatten)
    res <- dplyr::bind_rows(res)
    return(res)
}


#' Converts the results of a pull expression query
#'
#' @param query.res A \code{list} of \code{data.frames}. The results of the query
#' @param flatten If \code{TRUE} nested \code{data.frames} are recursively flattened
#'   using \code{wick::flatten_data_frame}
#' @param exclude.db.ids If \code{TRUE}, columns representing db identifiers are removed
#'   from the result
#' @return Returns a \code{data.frame} of converted query results
#'
#' @export
#'
convert_pull_query_results <- function(query.res, flatten = TRUE,
                                       exclude.db.ids = TRUE) {
    res <- query.res

    if(flatten && length(res) > 0) {

        res <- flatten_data_frame(res)

        names(res) <- sapply(strsplit(names(res), "\\."), function(x) {
            n <- length(x)
            if(x[n] == ":db/ident")
                x[n - 1]
            # Preserve references that are not being followed
            else if(n > 1 && x[n] == ":db/id")
                paste(x, collapse = ".")
            else
                x[n]
        })

        if(exclude.db.ids){
            exclude.cols <- grepl("^:db/id|db/ident|uid$", names(res))
            res <- res[, !exclude.cols, drop = FALSE]
        }

        names(res) <- gsub(":", "", names(res))
        names(res) <- make.names(names(res))

        # This can happen in cases where there is a map in the pull specification for an attribute
        # but some entities miss the attribute. In that case the entities missing the
        # attribute will have NA, while the other entities will have whatever attributes are included
        # from the map specification. Therefore when the data.frames are combined with dplyr::bind_rows
        # this will generate a column with all NAs, which can be removed
        res <- res[, !sapply(res, function(x) {all(is.na(x))}), drop = FALSE]
    }
    return(res)

}


#' Converts query results
#'
#' This function converts query results so that they can be more easily consumed by R.
#' The data is transformed into a \code{data.frame}, and variables are converted
#' to \code{numeric} when possible.
#'
#' @param query The query that generated this result
#' @param x The query result
#' @param simplify If \code{TRUE} a \code{data.frame} with a single column
#'   is simplified to a vector
#'
#' @return Returns the query results in \code{data.frame} format. Column names are derived
#'   from the \code{find} clause of the query
convert_query_results <- function(query, x, simplify = TRUE) {
    col.names <- unlist(query$query$find)
    col.names <- col.names[grep("^\\?", col.names)] # Remove non-symbol strings such as max, min etc.
    col.names <- gsub("\\?", "", col.names)
    col.names <- make.names(col.names)


    x <- data.frame(x, stringsAsFactors = FALSE)
    x <- data.frame(lapply(x, try_numeric), stringsAsFactors = FALSE)

    if (nrow(x) > 0){
        names(x) <- col.names

        if(ncol(x) == 1 && simplify)
            x <- x[, 1]
    } else {
        x <- setNames(data.frame(matrix(ncol = length(col.names), nrow = 0)),
                      col.names)
    }

    return(x)
}




