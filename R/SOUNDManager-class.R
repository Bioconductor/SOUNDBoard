#' @rdname SOUNDManager-class
#'
#' @aliases show,SOUNDBoard-method
#'
#' @title Managing SOUNDBoard Reports
#'
#' @description This page describes facilities for managing SOUNDBoard
#'     reports. Management tasks include (1) creating template
#'     reports; (2) publishing as temporary documents during
#'     development; and (3) publishing as final reports for
#'     communciation with board members.
#'
#' @exportClass SOUNDManager
.SOUNDManager <- setClass(
    "SOUNDManager",
    contains = "SOUNDBoard",
    slots = c(
        ## development
        board_directory = "character",
        sql_template_path = "character",
        ## production
        host = "character",
        port = "character",
        path = "character",
        username = "character"
    )
)

#' @rdname SOUNDManager-class
#'
#' @param board_directory character(1) Local directory where board, case,
#'     and assay elements are marshalled.
#'
#' @param sql_template_path character(1) path to existing SQL template
#'     file, containing table and templated query definitions.
#'
#' @param host character(1) URI of deployment server directory.
#'
#' @param port character(1) communation endpoint from the host.
#'
#' @param path character(1) path to directory where SOUNDBoard
#'     reports are served.
#'
#' @param username character(1) name of SOUNDBoard user from which
#'     applications are deployed. Default: "soundboard"
#'
#' @export
SOUNDManager <-
    function(
        ## development
        board_directory, sql_template_path,
        ## production
        host = "localhost", port = "3838", path = "SOUNDBoard",
        username = "soundboard"
    )
{
    manager <- .SOUNDManager_development(
        .SOUNDManager(), board_directory, sql_template_path
    )
    .SOUNDManager_production(manager, host, port, path, username)
}

.SOUNDManager_development <-
    function(manager, board_directory, sql_template_path)
{
    stopifnot(.is_scalar_character(board_directory))
    if (!dir.exists(board_directory))
        dir.create(board_directory)
    sql_file <- file.path(board_directory, .SQL_FILENAME)

    if (missing(sql_template_path)) {
        sql_template_path <- system.file(
            package="SOUNDBoard", "template", "SOUNDBoard.sql"
        )
    }
    stopifnot(
        .is_scalar_character(sql_template_path),
        file.exists(sql_template_path)
    )
    if (!missing(sql_template_path) && file.exists(sql_file))
        message("re-using existing board_directory ", basename(sql_file))

    ## create data base
    if (!file.exists(sql_file)) {
        cmds <- .sql_template_cmds(sql_template_path)
        cmds <- cmds[endsWith(cmds, "_TABLE")]
        cmds <- vapply(
            cmds, .sql_sprintf, character(1),
            sql_cmd_template = sql_template_path
        )
        result <- .sql_get_query(sql_file, cmds)
        .sql_templates_create_insert(sql_file, sql_template_path)
    }

    initialize(
        manager, board_directory = board_directory,
        sql_template_path = sql_template_path
    )
}

.SOUNDManager_production <-
    function(manager, host, port, path, username)
{
    stopifnot(
        .is_scalar_character(host),
        .is_scalar_character(as.character(port)),
        .is_scalar_character(path),
        .is_scalar_character(username)
    )

    initialize(
        manager, host = host, port = port, path = path, username = username
    )
}

.board_directory <- function(object) object@board_directory

.sql_template_path <- function(object) object@sql_template_path

.sql_file <- function(object)
    file.path(.board_directory(object), .SQL_FILENAME)

.host <- function(object) object@host

.port <- function(object) object@port

.path <- function(object) object@path

.username <- function(object) object@username

##
## sql-related
##

.src_sqlite <-
    function(x)
{
    src_sqlite(.sql_file(x))
}

#' @importFrom dplyr src_sqlite src_tbls
src_tbls.SOUNDManager <-
    function(x)
{
    tbls <- src_tbls(.src_sqlite(x))
    setdiff(tbls, "sqlite_sequence")
}

#' @importFrom dplyr tbl
#'
#' @export
tbl.SOUNDManager <-
    function(src, from, ...)
{
    tbl <- tbl(.src_sqlite(src), from, ...)
    tbl$board_directory <- .board_directory(src)
    class(tbl) <- c(paste0("tbl_", from), "tbl_sound", class(tbl))
    idx <- which(!endsWith(colnames(tbl), "_"))
    select(tbl, idx)
}

#' @export
`tbl<-` <- function(src, from, ..., value)
    UseMethod("tbl<-")

#' @export
`tbl<-.SOUNDManager` <-
    function(src, from, ..., value)
{
    .stopifnot_is_scalar(from)
    stopifnot(from %in% src_tbls(src))
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
    result <- .sql_execute(
        .sql_file(src), .sql_template_path(src), sql_cmd, value
    )

    src
}


##
## manager functionality
##

#' @importFrom BiocFileCache BiocFileCache bfcnew
#'
#' @export
manage <-
    function(x, resource)
{
    if (!is(resource, "SOUNDWidget"))
        resource <- RDSWidget(resource)
    fl <- bfcnew(BiocFileCache(.board_directory(x)), class(resource))
    .save(resource, fl)
    names(fl)
}

#' @rdname SOUNDManager-class
#'
#' @param object A \code{SOUNDManager} instance.
#'
#' @export
setMethod("show", "SOUNDManager",
    function(object)
{
    callNextMethod()
    cat(
        "development:",
        "\n  board_directory: ", .board_directory(object),
        "\n  sql_template: ", .sql_template_path(object),
        "\nproduction:",
        "\n  url: https://", .host(object), ":", .port(object),
            "/", .path(object),
        "\n  username: ", .username(object),
        "\ntbl(): ", paste(src_tbls(object), collapse=", "), "\n",
        sep=""
    )
})
