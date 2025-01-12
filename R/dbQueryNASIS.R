#' Query a NASIS DBIConnection
#'
#' Send queries to a NASIS DBIConnection
#'
#'
#' @param conn A \code{DBIConnection} object, as returned by \code{DBI::dbConnect()}.
#' @param q A statement to execute using \code{DBI::dbGetQuery}; or a (named) vector containing multiple statements to evaluate separately
#' @param close Close connection after query? Default: \code{TRUE}
#' @param ... Additional arguments to \code{DBI::dbGetQuery}
#' @return Result of \code{DBI::dbGetQuery}
#' @export dbQueryNASIS
dbQueryNASIS <- function(conn, q, close = TRUE, ...) {

  if (inherits(conn, 'try-error'))
    stop("Failed to connect to NASIS database!")

  if (close) {
    # don't close connections we tagged as user-defined
    isUserDefined <- attr(conn, 'isUserDefined')
    if(!is.null(isUserDefined) && isUserDefined) {
      close <- FALSE
    }
  }
  
  # vectorize queries (return a [possibly named] list)
  if(length(q) > 1) {
    # recursively call dbQueryNASIS(close=FALSE)
    res <- lapply(q, function(x) dbQueryNASIS(conn, x, close = FALSE))
    names(res) <- names(q)
    dd <- res
  } else {
    ## exec query
    d <- DBI::dbGetQuery(conn, q, ...)

    dd <- data.frame(d)

  }

  ## close connection if needed
  if (close) {
    DBI::dbDisconnect(conn)
  }
  return(dd)
}


