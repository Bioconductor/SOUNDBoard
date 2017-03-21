resources <- soundboard2.css <- NULL

.onLoad <- function (...) {
    resources <<- system.file(package = "SOUNDBoard", "resources")
    soundboard2.css <<- file.path(resources, "html", "soundboard2.css")
}
