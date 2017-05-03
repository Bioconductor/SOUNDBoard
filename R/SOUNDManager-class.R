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
        rmd_template_path = "character",
        ## production
        user = "character",
        host = "character",
        deploy_path = "character",
        port = "character",
        path = "character"
    )
)

#' @rdname SOUNDManager-class
#'
#' @param board_directory character(1) Local directory where board, case,
#'     and assay elements are marshalled.
#'
#' @param sql_template_path character(1) path to existing SQL template
#'     file, containing table and templated query definitions. The
#'     default is \code{system.file(package="SOUNDBoard", "template",
#'     "SOUNDBoard.sql")} (also use this file to model your own
#'     templates).
#'
#' @param rmd_template_path character(1) (optional) path to existing
#'     Rmd template file, containing an R-markdown file to be
#'     displayed to SOUNDBoard members. If no Rmd files exist in
#'     \code{board_directory} and \code{rmd_template_path} is missing,
#'     \code{system.file(pacakge="SOUNDBoard", "template",
#'     "SOUNDBoard.Rmd")} is copied to \code{board_directory}. If a
#'     file with \code{basename(rmd_template_path)} already exists in
#'     \code{board_directory}, a warning is issued and the existing
#'     file \emph{not} replaced.
#'
#' @param user character(1) name of SOUNDBoard user from which
#'     applications are deployed. Default: "soundboard"
#'
#' @param host character(1) URI of deployment server directory.
#'
#' @param port character(1) communation endpoint from the host.
#'
#' @param deploy_path character(1) server path to directory where
#'     SOUNDBoard reports are deployed to.
#'
#' @param path character(1) http: path to SOUNDBoard reports.
#'
#' @export
SOUNDManager <-
    function(
        ## development
        board_directory, sql_template_path, rmd_template_path,
        ## production
        user = "soundboard", host = "localhost",
        deploy_path = file.path("~/srv", basename(board_directory)),
        port = "3838", path=""
    )
{
    manager <- .SOUNDManager_development(
        .SOUNDManager(), board_directory, sql_template_path, rmd_template_path
    )
    .SOUNDManager_production(manager, user, host, deploy_path, port, path)
}

.SOUNDManager_development <-
    function(manager, board_directory, sql_template_path, rmd_template_path)
{
    stopifnot(
        .is_scalar_character(board_directory)
    )
    if (!dir.exists(board_directory))
        dir.create(board_directory)

    sql_file <- file.path(
        board_directory, paste0(basename(board_directory), ".sqlite")
    )

    if (missing(sql_template_path)) {
        sql_template_path <- system.file(
            package="SOUNDBoard", "template", .SQL_TEMPLATE
        )
    }
    stopifnot(
        .is_scalar_character(sql_template_path),
        file.exists(sql_template_path)
    )
    if (!missing(sql_template_path) && file.exists(sql_file))
        .sbmessage("re-using existing SQLite file ", basename(sql_file))

    if (missing(rmd_template_path)) {
        rmd_template_path <- system.file(
            package="SOUNDBoard", "template", .RMD_TEMPLATE
        )
    }
    stopifnot(
        .is_scalar_character(rmd_template_path),
        file.exists(rmd_template_path)
    )
    rmd_files <- dir(board_directory, pattern=".Rmd$", full.names=TRUE)
    rmd_idx <- basename(rmd_files) == basename(rmd_template_path)
    if (any(rmd_idx) && !missing(rmd_template_path))
        .sbmessage("re-using existing Rmd file ", basename(rmd_template_path))

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
    } else {
        ## FIXME: insert statements need to be created from existing sql file
    }

    ## create Rmd
    if (!any(rmd_idx)) {
        path <- file.path(
            board_directory,
            paste0(basename(board_directory), ".Rmd")
        )
        file.copy(rmd_template_path, path)
    } else {
        rmd_template_path <- rmd_files[rmd_idx]
    }

    ## copy css
    css <- system.file(
        package="SOUNDBoard", "resources", "html", "soundboard2.css"
    )
    file.copy(css, board_directory)

    initialize(
        manager, board_directory = board_directory,
        sql_template_path = sql_template_path,
        rmd_template_path = rmd_template_path
    )
}

.SOUNDManager_production <-
    function(manager, user, host, deploy_path, port, path)
{
    stopifnot(
        .is_scalar_character(user),
        .is_scalar_character(host),
        .is_scalar_character(deploy_path),
        .is_scalar_character(as.character(port)),
        .is_scalar_character(path, zok = TRUE)
    )

    initialize(
        manager, host = host, user = user, deploy_path = deploy_path,
        port = port, path = path
    )
}

##
## internal accessors
##

.board_directory <- function(object) object@board_directory

.sql_template_path <- function(object) object@sql_template_path

.rmd_template_path <- function(object) object@rmd_template_path

.sql_file <- function(object) {
    path <- .board_directory(object)
    file.path(path, paste0(basename(path), ".sqlite"))
}

.user <- function(object) object@user

.host <- function(object) object@host

.deploy_path <- function(object) object@deploy_path

.port <- function(object) object@port

.path <- function(object) object@path

##
## exported accessors
##

#' @rdname SOUNDManager-class
#'
#' @export
deploy_path <-
    function(object)
{
    paste0(.user(object), "@", .host(object), ":", .deploy_path(object), "/")
}

#' @rdname SOUNDManager-class
#'
#' @export
urls <-
    function(object)
{
    paste0(
        "http://", .host(object), ":", .port(object), "/",
        if (nzchar(.path(object))) paste0(.path(object), "/"),
        basename(.board_directory(object)), "/",
        dir(.board_directory(object), ".Rmd$")
    )
}

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
#' @param dryrun logical(1) rsync to \code{deploy_path()}?
#'
#' @param options character(1) rsync options.
#'
#' @export
deploy <-
    function(x, dryrun = TRUE, options = "-avz")
{
    stopifnot(
        .is_scalar_logical(dryrun),
        .is_scalar_character(options, zok=TRUE)
    )

    cmd <- "rsync"
    args <- c(
        if (dryrun) "--dry-run",
        options,
        "-e ssh",
        paste0(.board_directory(x), "/"),
        deploy_path(x)
    )
    .sbmessage(cmd, " ", paste(args, collapse=" "))
    status <- system2(cmd, args)
    if (status != 0L)
        stop("failed to deploy project\n  error code: ", status)
    invisible(status)
}

#' @rdname SOUNDManager-class
#'
#' @param object \code{SOUNDManager-class} instance.
#'
#' @export
setMethod("show", "SOUNDManager",
    function(object)
{
    callNextMethod()
    rmd <- dir(.board_directory(object), ".Rmd$")
    reports <- paste(c("reports:", rmd), collapse=" ")

    cat(
        "templates:",
        "\n  sql: ", .sql_template_path(object),
        "\n  rmd: ", .rmd_template_path(object),
        "\ndevelopment:",
        "\n  board_directory: ", .board_directory(object),
        "\n  sqlite: ", basename(.sql_file(object)),
        "\n", paste0(strwrap(reports, indent=2, exdent=4), collapse="\n"),
        "\nproduction:",
        "\n  deploy_path(): ", deploy_path(object),
        "\n  urls(): ",
        "\n    ", paste(urls(object), collapse="\n    "),
        "\ntbl(): ", paste(src_tbls(object), collapse=", "), "\n",
        sep=""
    )
})
