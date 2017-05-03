.stopifnot_is_scalar <-
    function(object, id)
{
    test <- length(object) == 1L && !is.na(object)
    if (!test) {
        if (missing(id))
            id <- deparse(substitute(object))
        stop("'", id, "' must be character(1) and not 'NA'", call.=FALSE)
    }
}

.stopifnot_scalar_character <-
    function(object, id)
{
    if (!.is_scalar_character(object)) {
        if (missing(id))
            id <- deparse(substitute(object))
        stop("'", id, "' must be character(1) and not 'NA'", call.=FALSE)
    }
}

.is_scalar_logical <-
    function(object, naok = FALSE)
{
    is.logical(object) && length(object) == 1L && (naok || !is.na(object))
}

.is_scalar_character <-
    function(object, naok = FALSE, zok = FALSE)
{
    is.character(object) && length(object) == 1L &&
        (naok || !is.na(object)) && (zok || nzchar(object))
}

## SOUNDCase

.stopifnot_allowed_key <-
    function(object, id, allowed_keys)
{
    if (!length(allowed_keys))
        return()
    if (!object %in% allowed_keys) {
        if (missing(id))
            id <- deparse(substitute(object))
        stop("'", id, "' not in allowed_keys", call.=FALSE)
    }
}

## .sbmessage
.sbmessage <-
    function(...)
{
    if (interactive())
        message(...)
}
