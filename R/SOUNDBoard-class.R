## Base class for package
.SOUNDBoard <- setClass(
    "SOUNDBoard",
    contains = "VIRTUAL"
)

#' @import methods
#' 
#' @export
setMethod("show", "SOUNDBoard",
    function(object)
{
    cat("class:", class(object), "\n")
})
