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
    slots = c(
        resource = "ANY",
        save = "function",
        load = "function",
        report = "function"
    ),
    prototype = list(
        save = function(x, file) saveRDS(x, file),
        load = function(file) readRDA(file),
        report = function(x) stop("'report()' not implemented")
    )
)

.resource <- function(x) x@resource

.save <-
    function(x, file)
{
    tryCatch({
        x@save(.resource(x), file)
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
    resource <- tryCatch({
        x@load(file)
    }, error = function(err) {
        stop(
            "\n'", class(x), "' cannot load resource:",
            "\n  ", conditionMessage(err)
        )
    })
    initialize(x, resource=resource)
}

.report <-
    function(x)
{
    tryCatch({
        x@report(.resource(x))
    }, error = function(err) {
        stop(
            "\n'", class(x), "' cannot generate report():",
            "\n  ", conditionMessage(err)
        )
    })
}

#' @importFrom methods setClass
#'
#' @export
SOUNDWidget <-
    function(name, save, load, report)
{
    class <- setClass(
        name, contains="SOUNDWidget",
        prototype = prototype(
            save = save,
            load = load,
            report = report
        )
    )
    
    function(resource = NULL) {
        class(resource=resource)
    }
}

#' @export
RDSWidget <- SOUNDWidget(
    "RDSWidget",
    save = function(x, file) saveRDS(x, file),
    load = function(file) readRDS(file),
    report = function(x) report(x)
)
