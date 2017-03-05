.stopifnot_scalar_character <-
    function(object)
{
    test <- is.character(object) && length(object) == 1L && !is.na(object)
    if (!test) {
        arg <- deparse(substitute(object))
        stop("'", arg, "' must be character(1) and not 'NA'",
             call.=FALSE)
    }
}
