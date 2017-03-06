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
    contains = "SOUNDBoard"
)

#' @rdname SOUNDWidget-class
#'
#' @exportClass SOUNDSimpleCase
.SOUNDSimpleCase <- setClass(
    "SOUNDSimpleCase",
    contains = "SOUNDWidget",
    slots = c(
        keyvalue = "list",
        allowed_keys = "character"
    )
)

#' @rdname SOUNDWidget-class
#' 
#' @details Use \code{SOUNDSimpleCase()} to construct a Case instance
#'     of simple key-value pairs. Each key must be unique. Each value
#'     must be a scalar. When specified, \code{allowed_keys} restricts
#'     key names to specific values.
#' 
#' @export
SOUNDSimpleCase <-
    function(..., allowed_keys = character())
{
    args <- list(...)
    stopifnot(length(args) > 0L)
    stopifnot(
        is.character(names(args)), length(names(args)) == length(args),
        !any(duplicated(names(args))), all(nzchar(names(args)))
    )
    for (nm in names(args)) {
        .stopifnot_allowed_key(nm, nm, allowed_keys)
        .stopifnot_is_scalar(args[[nm]], nm)
    }

    .SOUNDSimpleCase(keyvalue  = args)
}        

.keyvalue <- function(object) object@keyvalue

.allowed_keys <- function(object) object@allowed_keys

#' @rdname SOUNDWidget-class
#'
#' @aliases show,SOUNDSimpleCase-class
#'
#' @param object A SOUNDWidget object
#'
#' @export
setMethod("show", "SOUNDSimpleCase",
    function(object)
{
    callNextMethod()
    kv <- .keyvalue(object)
    for (key in names(kv))
        cat(key, ": ", kv[[key]], "\n", sep="")
})
