resources <- soundboard2.css <- NULL

.onLoad <- function (...) {
    resources <<- system.file(package = "SOUNDBoard", "resources")
    soundboard2.css <<- file.path(resources, "html", "soundboard2.css")
}

.options <- local({
    options <- c("sequential", "diverging", "qualitative")
    env <- new.env(parent=emptyenv())
    list(set=function(variable, value) {
        stopifnot(variable %in% options)
        if (!value %in% rownames(RColorBrewer::brewer.pal.info))
            stop("Select a valid color palette")
        env[[variable]] <- value
    }, get=function(variable) {
        stopifnot(variable %in% options)
        env[[variable]]
    })
})

.options$set("sequential", "GnBu")
.options$set("diverging", "RdBu")
.options$set("qualitative", "Paired")
