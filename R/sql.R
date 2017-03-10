#' @import RSQLite
#' @importFrom stats setNames

.SQL_CACHE <- local({
    new.env(parent=emptyenv())
})

.SQL_FILENAME <- "SOUNDBoard.sqlite"

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

.sql_query <-
    function(sql_file, sqls)
{
    conn <- dbConnect(SQLite(), sql_file)
    result <- lapply(sqls, dbGetQuery, conn = conn)
    dbDisconnect(conn)
    result
}
