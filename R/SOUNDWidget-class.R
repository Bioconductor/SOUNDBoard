#' @rdname SOUNDWidget-class
#'
#' @title Create and use 'widgets' for consistent data presenation
#'
#' @description This page describes 'widgets' for representating and
#'     reporting elements of a SOUND Board Report. Each widget
#'     describes a particular component of the data, e.g., the
#'     'metadata' about the case; a top table of differentially
#'     expressed genes, an interactive plot of variant effects. The
#'     widget provides a constructor method, as well as a 'render'
#'     method. Widgets themselves may perform considerable work when
#'     created, but render quickly. Widgets can be fully serialized.
#'
#' @param ... key-value pairs of case information. Arguments must be
#'     named and unique. Each value must be a scalar. When
#'     \code{length(allowed_keys) != 0}, names of keys must be in
#'     \code{allowed_keys}.
#'
#' @param allowed_keys character() of allowed key names.
#'
#' @exportClass SOUNDWidget
.SOUNDWidget <- setClass(
    "SOUNDWidget",
    contains = "SOUNDBoard",
    slots = c(resource = "ANY")
)

sbresource <- function(x) x@resource

.save <-
    function(x, file)
{
    tryCatch({
        sbsave(x, file)
    }, error = function(err) {
        stop(
            "\n'", class(x), "' cannot save resource:",
            "\n  ", conditionMessage(err)
        )
    })
}

.load <-
    function(x, file)
{
    tryCatch({
        resource <- sbload(x, file)
        if (!is(resource, class(x)))
            stop(
                "'sbload()' returned '", class(resource), "'",
                ", expected '", class(x), "'"
            )
        resource
    }, error = function(err) {
        stop(
            "\n'", class(x), "' cannot load resource:",
            "\n  ", conditionMessage(err)
        )
    })
}

.report <-
    function(x)
{
    tryCatch({
        sbreport(x)
    }, error = function(err) {
        stop(
            "\n'", class(x), "' cannot generate sbreport():",
            "\n  ", conditionMessage(err)
        )
    })
}

#' @rdname SOUNDWidget-class
#' 
#' @param where The environment in which to define a
#'     SOUNDWidget-derived class and methods.
#'
#' @importFrom methods setClass setMethod
#'
#' @export
SOUNDWidget <-
    function(widget, save, load, report, ..., where=.GlobalEnv)
{
    class <- setClass(widget, contains="SOUNDWidget", ..., where=where)
    constructor <- function(resource = NULL)
        class(resource=resource)

    if (!missing(save))
        setMethod(sbsave, widget, save, where=where)
    if (!missing(load))
        setMethod(sbload, widget, load, where=where)
    if (!missing(report))
        setMethod(sbreport, widget, report, where=where)

    constructor
}

#' @exportClass RDSWidget
#'
#' @export
RDSWidget <- SOUNDWidget(
    "RDSWidget",
    save = function(x, file) saveRDS(x, file),
    load = function(x, file) readRDS(file),
    report = function(x) sbreport(sbresource(x)),
    where = topenv()
)
