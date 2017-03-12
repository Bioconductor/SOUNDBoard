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
    test <- is.character(object) && length(object) == 1L && !is.na(object)
    if (!test) {
        if (missing(id))
            id <- deparse(substitute(object))
        stop("'", id, "' must be character(1) and not 'NA'", call.=FALSE)
    }
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
