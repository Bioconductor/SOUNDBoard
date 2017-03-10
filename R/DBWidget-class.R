#' @rdname DBWidget-class
#'
#' @title Create and use a data base (sqlite)-backed widget
#'
#' @description This page describes a widget for working with data
#'     base backed resources. A typical use case is to manage a
#'     collection of board, case and assay results.
#'
#' @param ... key-value pairs of case information. Arguments must be
#'     named and unique. Each value must be a scalar. When
#'     \code{length(allowed_keys) != 0}, names of keys must be in
#'     \code{allowed_keys}.
#'
#' @param allowed_keys character() of allowed key names.
#'
#' @exportClass DBWidget
.DBWidget <- setClass(
    "DBWidget",
    contains = "SOUNDWidget",
    slots = c(
        sql_cmd_template = "character",
        db_directory = "character"
    )
)

#' @rdname DBWidget-class
#'
#' @description
#'
#' \code{DBWidget()}: create an instance of a data base widget.
#'
#' @param sql_cmd_template character(1) path to existing SQL template
#'     file, containing table and templated query definitions.
#'
#' @param sql_file character(1) path to SQL file. If the file does not
#'     exist, the template from \code{sql_cmd_template} is used to
#'     create the data. If the data base exists, no checks are
#'     performed to validate atains the provide
#'     \code{sql_cmd_template}.
#'
#' @export
DBWidget <-
    function(db_directory, sql_cmd_template)
{
    if (missing(sql_cmd_template))
        sql_cmd_template <- system.file(package="SOUNDBoard", "template")
    .stopifnot_scalar_character(sql_cmd_template)
    sql_cmd_template <- file.path(sql_cmd_template, "SOUNDBoard.sql")
    stopifnot(file.exists(sql_cmd_template))

    .stopifnot_scalar_character(db_directory)
    if (!dir.exists(db_directory))
        dir.create(db_directory)
    sql_file <- file.path(db_directory, .SQL_FILENAME)

    ## create data base, if necessary
    if (!file.exists(sql_file)) {
        cmds <- .sql_template_cmds(sql_cmd_template)
        cmds <- cmds[endsWith(cmds, "_TABLE")]
        cmds <- vapply(
            cmds, .sql_sprintf, character(1),
            sql_cmd_template = sql_cmd_template
        )
        result <- .sql_query(sql_file, cmds)
    }
    .DBWidget(sql_cmd_template = sql_cmd_template, db_directory = db_directory)
}

.sql_cmd_template <- function(object) object@sql_cmd_template

.db_directory <- function(object) object@db_directory

.sql_file <- function(object) file.path(.db_directory(object), .SQL_FILENAME)

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

#' @rdname DBWidget-class
#'
#' @export
tbl.DBWidget <-
    function(src, from, ...)
{
    tbl <- tbl(sql_file(src), from, ...)
    tbl$db_directory <- .db_directory(src)
    tbl
}

#' @export
`tbl<-` <- function(src, from, ..., value)
    UseMethod("tbl<-")

#' @export
`tbl<-.DBWidget` <-
    function(src, from, ..., value)
{
    .stopifnot_is_scalar(from)
    stopifnot(from %in% src_tbls(sql_file(x)))
    stopifnot(
        is.list(value),
        setequal(names(value), colnames(tbl(src, from)))
    )

    value <- lapply(value, function(v) {
        if (is.character(v))
            gsub("[[:space:]]+", " ", v)
        v
    })

    sql_cmd <- sprintf("-- %s_INSERT", toupper(from))
    args <- c(list(.sql_cmd_template(src), sql_cmd), value)
    cmd <- do.call(".sql_sprintf", args)

    .sql_query(.sql_file(x), cmd)

    src
}               

#' @rdname DBWidget-class
#'
#' @aliases show,DBWidget-method
#' 
#' @export
setMethod("show", "DBWidget",
    function(object)
{
    callNextMethod()
    cat("sql_file(): use tbl(<dbwidget>, <tbl name>)\n")
    print(sql_file(object))
})

#' @importFrom BiocFileCache BiocFileCache bfcnew
#'
#' @export
manage <-
    function(x, resource)
{
    fl <- bfcnew(BiocFileCache(.db_directory(x)), "RDS")
    saveRDS(resource, fl)
    names(fl)
}

#' @export
report <- function(x, ...) NextMethod("report")

#' @export
report.tbl_board <-
    function(x, ...)
{
    df <- as.data.frame(x)
    cat("**Board**: ", df$board_uid, "\n", sep="")
}


#' @export
report.tbl_cases <-
    function(x, ...)
{
    df <- as.data.frame(x)
    if ("case_uid" %in% names(df))
        cat("<br>**Case**: ", df$case_uid, "\n")
    df <- df[names(df) != "case_uid"]
    for (nm in names(df))
        cat("<br>", sub("^(.)", "\\U\\1", nm, perl=TRUE), ": ",
            df[[nm]], "\n", sep="")
}

#' @importFrom BiocFileCache BiocFileCache bfcinfo
#' @export
report.tbl_assay <-
    function(x, ...)
{
    bfcid <- as.data.frame(x)$resource
    df <- as.data.frame(bfcinfo(BiocFileCache(x$db_directory)[bfcid]))
    resource <- switch(
        df$rname,
        RDS = readRDS(df$rpath)
    )

    report(resource)
}

#' @importFrom DT datatable
#' @export
report.data.frame <-
    function(x, ...)
{
    DT::datatable(x, ...)
}

#' @importFrom plotly ggplotly 
#' @export
report.gg <-
    function(x, ...)
{
    plotly::ggplotly(x, ...)
}
