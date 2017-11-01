#' @rdname sbsave-load-report
#'
#' @title Save, load, and report on SOUNDWidget instances.
#'
#' @param x An instance derived from \code{SOUNDWidget-class}.
#'
#' @param file character(1) Location to or from which the instance
#'     \code{x} is to be saved / loaded.
#'
#' @description These methods are usually created for the user with
#'     \code{SOUNDWidget()}, they are not created directly. Likewise
#'     methods are invoked by SOUNDBoard infrastructure, not by the
#'     user.
#'
#' @details
#'
#' An \code{sbsave(x, file)} method saves \code{x} to \code{file}.
#'
#' An \code{sbload(x, file)} method creates an instance of class
#' \code{x} from location \code{file}. The instance \code{x} passed to
#' the mehod is created by the default constructor of \code{x}, and
#' does \emph{not} contain the resource. A typical method will input
#' the data \code{resource <- ...}, and then populate the instance
#' \code{initialize(x, resource = resource)}
#'
#' An \code{sbreport(x)} method creates output from \code{x} suitable
#' for inclusion in a SOUNDBoard report.
#'
#' @export
setGeneric(
    "sbsave",
    function(x, file) standardGeneric("sbsave"),
    signature = "x"
)

#' @rdname sbsave-load-report
#'
#' @export
setGeneric(
    "sbload",
    function(x, file) standardGeneric("sbload"),
    signature = "x"
)

#' @rdname sbsave-load-report
#'
#' @export
setGeneric(
    "sbreport",
    function(x) standardGeneric("sbreport")
)

##
## tbl_*
##

local({
    .base_class <- c("tbl_sqlite", "tbl_sql", "tbl_lazy", "tbl")

    setOldClass(c("tbl_board", "tbl_sound", .base_class))
    setOldClass(c("tbl_cases", "tbl_sound", .base_class))
    setOldClass(c("tbl_assay", "tbl_sound", .base_class))
})

#' @rdname sbsave-load-report
#'
#' @export
setMethod("sbreport", "tbl_board",
    function(x)
{
    df <- as.data.frame(x)
    cat("**Board**: ", df$board_uid, "\n", sep="")
})

#' @rdname sbsave-load-report
#'
#' @export
setMethod("sbreport", "tbl_cases",
    function(x)
{
    df <- as.data.frame(x)
    if ("case_uid" %in% names(df))
        cat("<br>**Case**: ", df$case_uid, "\n")
    df <- df[names(df) != "case_uid"]
    for (nm in names(df))
        cat("<br>", sub("^(.)", "\\U\\1", nm, perl=TRUE), ": ",
            df[[nm]], "\n", sep="")
})

#' @rdname sbsave-load-report
#'
#' @importFrom BiocFileCache BiocFileCache bfcinfo bfcrpath
#'
#' @export
setMethod("sbreport", "tbl_assay",
    function(x)
{
    bfcid <- as.data.frame(x)$resource
    bfc <- BiocFileCache(x$assets_directory)[bfcid]
    resource <- .load(
        new(as.data.frame(bfcinfo(bfc))$rname),
        bfcrpath(bfc)
    )
    .report(resource)
})

##
## built-in
##

#' @rdname sbsave-load-report
#'
#' @importFrom DT datatable
#'
#' @export
setMethod("sbreport", "data.frame",
    function(x)
{
    DT::datatable(x)
})

#' @rdname sbsave-load-report
#' @importClassesFrom S4Vectors DataFrame
#' @export
setMethod("sbreport", "DataFrame",
    function(x)
{
    DT::datatable(as.data.frame(x))
})

setOldClass(c("gg", "ggplot"))

#' @rdname sbsave-load-report
#'
#' @importFrom plotly ggplotly
#'
#' @export
setMethod("sbreport", "gg",
    function(x)
{
    plotly::ggplotly(x)
})

#' @rdname sbsave-load-report
#'
#' @importFrom qtlcharts iplotCorr iplotCorr_render
#'
#' @export
setMethod("sbreport", "iplotCorr",
    function(x)
{
    qtlcharts::iplotCorr_render(x)
})
