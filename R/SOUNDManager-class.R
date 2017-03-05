#' @rdname SOUNDManager-class
#' 
#' @aliases show,SOUNDBoard-method
#' 
#' @title Managing SOUNDBoard Reports
#'
#' @description This page describes facilities for managing SOUNDBoard
#'     reports. Management tasks include (1) creating template
#'     reports; (2) publishing as temporary documents during
#'     development; and (3) publishing as final reports for
#'     communciation with board members.
#'
#' @exportClass SOUNDManager
.SOUNDManager <- setClass(
    "SOUNDManager",
    contains = "SOUNDBoard",
    slots = c(
        server_host = "character",
        server_path = "character",
        username = "character"
    )
)

#' @rdname SOUNDManager-class
#' 
#' @param server_host character(1) URI of deployment server directory.
#'
#' @param server_path character(1) path to directory where SOUNDBoard
#'     reports are served.
#'
#' @param username character(1) name of SOUNDBoard user from which
#'     applications are deployed. Default: "soundboard"
#'
#' @export
SOUNDManager <-
    function(
        server_host = character(), server_path = character(),
        username = "soundboard"
    )
{
    if (missing(server_host))
        server_host <- getSOUNDBoardOption("server_host")
    .stopifnot_scalar_character(server_host)

    if (missing(server_path))
        server_path <- getSOUNDBoardOption("server_path")
    .stopifnot_scalar_character(server_path)

    if (missing(username))
        username <- getSOUNDBoardOption("username")
    .stopifnot_scalar_character(username)

    .SOUNDManager(
        server_host = server_host, server_path=server_path, username = username
    )
}        

.server_host <- function(object) object@server_host

.server_path <- function(object) object@server_path

.username <- function(object) object@username

#' @rdname SOUNDManager-class
#'
#' @param object A \code{SOUNDManager} instance.
#' 
#' @export
setMethod("show", "SOUNDManager",
    function(object)
{
    callNextMethod()
    cat(
        "server_host:", .server_host(object),
        "\nserver_path:", .server_path(object),
        "\nusername:", .username(object),
        "\n"
    )
})    
