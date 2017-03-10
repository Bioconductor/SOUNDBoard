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
    resource <- .load(new(df$rname), df$rpath)
    .report(resource)
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
