#' @rdname DBWidget-class
#'
#' @param object An instance of class \code{DBWidget}
#'
#' @return \code{sql_file()} returns a dplyr 'src' object referencing
#'     the data base.
#' 
#' @import dplyr magrittr
#'
#' @export
sql_file <-
    function(object)
{
    src <- src_sqlite(.sql_file(object))
    class(src) <- c("src_sound", class(src))
    src
}

#' @rdname DBWidget-class
#'
#' @param x An instance of class \code{DBWidget}.
#'
#' @return \code{src_tbls()} returns the names of user-accessible
#'     tables available in \code{x}.
#'
#' @export
src_tbls.src_sound <-
    function(x)
{
    tbls <- NextMethod()
    setdiff(tbls, "sqlite_sequence")
}

#' @rdname DBWidget-class
#'
#' @param src A \code{DBWidget} or \code{src_sound} instance.
#'
#' @return \code{tbl()} returns the dplyr sqlite table corresponding
#'     to \code{from}.
#'
#' @export
tbl.src_sound <-
    function(src, from, ...)
{
    tbl <- NextMethod()
    tbl
    nms <- which(!endsWith(colnames(tbl), "_"))
    tbl <- select(tbl, nms)
    class(tbl) <- c(paste("tbl", from, sep="_"), "tbl_sound", class(tbl))
    tbl
}

