#' Create local NASIS database connection
#'
#' Create a connection to a local NASIS database with `DBI`
#'
#' @aliases NASIS
#' @param dsn Optional: path to SQLite database containing NASIS table
#' structure; Default: \code{NULL}
#' @return A \code{DBIConnection} object, as returned by
#' \code{DBI::dbConnect()}. If `dsn` is a `DBIConnection`, the attribute `isUserDefined` of the result is set to `TRUE`. If the `DBIConnection` is created by the internal NASIS connection process, `isUserDefined` is set to `FALSE.`
#' @export dbConnectNASIS
#' @rdname dbConnectNASIS
dbConnectNASIS <- function(dsn = NULL) {
  
  # allow users to set their custom DBI connection with dsn argument
  isUserDefined <- inherits(dsn, 'DBIConnection')
  
  if (isUserDefined) {
    attr(dsn, 'isUserDefined') <- TRUE
    return(dsn)
  }
  
  # default connection uses DBI/odbc, alternately RSQLite
  res <- .openNASISchannel(dsn)
  attr(res, 'isUserDefined') <- FALSE
  
  return(res)
}

# shorthand
#' @export
#' @rdname dbConnectNASIS
NASIS <- function(dsn = NULL) dbConnectNASIS(dsn = dsn)
