#' @import RSQLite
#' @importFrom stats setNames

.SQL_CACHE <- new.env(parent=emptyenv())

.SQL_TEMPLATE <- "SOUNDBoard.sql"

.SQL_INSERT_FMT = "INSERT INTO %s (%s) VALUES (%s)"

.SQLITE_FILE <- "SOUNDBoard.sqlite"

.sql_templates <-
    function(sql_cmd_template)
{
    if (!exists(sql_cmd_template, envir=.SQL_CACHE)) {
        sql_cmds <- readLines(sql_cmd_template)
        sql_cmds <- sub("^\b*--\b*$", "", sql_cmds)
        sql_cmds <- sql_cmds[nzchar(sql_cmds)]
        grps <- cumsum(grepl("^--", sql_cmds))
        body <- duplicated(grps)
        nms <- sql_cmds[!body]
        .SQL_CACHE[[sql_cmd_template]] <-
            setNames(split(sql_cmds[body], grps[body]), nms)
    }
    .SQL_CACHE[[sql_cmd_template]]
}

.sql_templates_create_insert <-
    function(sql_file, sql_cmd_template)
{
    conn <- dbConnect(SQLite(), sql_file)
    on.exit(dbDisconnect(conn))
    tbls <- setdiff(dbListTables(conn), "sqlite_sequence")
    for (tbl in tbls) {
        flds <- dbListFields(conn, tbl)
        flds <- flds[!endsWith(flds, "_")]
        insert <- sprintf(
            .SQL_INSERT_FMT, tbl,
            paste0(flds, collapse=", "),
            paste0(":", flds, collapse=", ")
        )
        nm <- paste0("-- ", toupper(tbl), "_INSERT")
        .SQL_CACHE[[sql_cmd_template]][[nm]] <- insert
    }
    .SQL_CACHE[[sql_cmd_template]]
}

.sql_template_cmds <-
    function(sql_cmd_template)
{
    names(.sql_templates(sql_cmd_template))
}

.sql_template <-
    function(sql_cmd_template, sql_cmd)
{
    tmpl <- .sql_templates(sql_cmd_template)
    stopifnot(sql_cmd %in% names(tmpl))
    tmpl[[sql_cmd]]
}

.sql_sprintf <-
    function(sql_cmd_template, sql_cmd, ...)
{
    tmpl <- .sql_template(sql_cmd_template, sql_cmd)
    cmds <- paste(tmpl, collapse="\n")
    sprintf(cmds, ...)
}


#' @importFrom DBI dbExecute
.sql_execute <-
    function(sql_file, sql_cmd_template, sql_cmd, params)
{
    conn <- dbConnect(SQLite(), sql_file)
    tmpl <- .sql_template(sql_cmd_template, sql_cmd)
    tmpl <- paste0(tmpl, collapse="")

    conn <- dbConnect(SQLite(), sql_file)
    result <- dbExecute(conn, tmpl, params = params)
    dbDisconnect(conn)

    result                              # number of rows modified
}

.sql_get_query <-
    function(sql_file, sqls)
{
    conn <- dbConnect(SQLite(), sql_file)
    result <- lapply(sqls, dbGetQuery, conn = conn)
    dbDisconnect(conn)
    result
}
