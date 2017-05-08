#' @import ggplot2
.onLoad <- function (...) {
    .options$set("sequential", "GnBu")
    .options$set("diverging", "RdBu")
    .options$set("qualitative", "Paired")
    .options$set("resources", system.file(package = "SOUNDBoard", "resources"))
    .options$set("css",
        file.path(.options$get("resources"), "html", "soundboard2.css"))

    theme_soundboard <- ggplot2::theme_bw() +
        ggplot2::theme(strip.background = element_rect(fill = "#12698C",
                                                       colour = "#3A8988"),
                       strip.text = element_text(colour = "#F0F0F0"),
                       legend.title = element_blank(),
                       panel.grid.major = element_line(colour = "#3A8988"))

    ggplot2::theme_set(theme_soundboard)
}

.options <- local({
    options <- list(
        colors = c("sequential", "diverging", "qualitative"),
        style = c("resources", "css")
    )
    env <- new.env(parent=emptyenv())

    list(set=function(variable, value) {
        stopifnot(
            is.character(variable), length(variable) == 1L, !is.na(variable),
            is.character(value), length(value) == 1L, !is.na(value),
            variable %in% unlist(options, use.names = FALSE)
        )

        if (variable %in% options[["colors"]] &&
            !value %in% rownames(RColorBrewer::brewer.pal.info))
            stop("Select a valid color palette")
        env[[variable]] <- value
    }, get=function(variable) {
        stopifnot(variable %in% unlist(options, use.names = FALSE))
        env[[variable]]
    })
})
