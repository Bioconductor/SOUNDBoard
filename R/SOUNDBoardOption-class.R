.SOUNDBoardOption <- local({
    env <- new.env(parent=emptyenv())
    env[[".options"]] <- c(
        "server_host", "server_path", "username"
    )
    env
})

#' @rdname SOUNDBoardOption-class
#'
#' @name SOUNDBoardOption
#' 
#' @title Manage SOUNDBoard options
#'
#' @description This page describes options available in the
#'     SOUNDBoard package. Options can be set with named key-value
#'     pairs, e.g., \code{SOUNDBoardOption(username =
#'     "soundboard")}. Options can also be specified by environment
#'     variables formed by the contatentation of \code{SOUNDBOARD_}
#'     and the option name, e.g., in bash \code{export
#'     SOUNDBOARD_USERNAME=soundboard}.
#'
#' @param ... key-value pairs of valid SOUNDBoard options.
#'     \itemize{
#' 
#'         \item{server_host} character(1) Deployment SOUNDBoard
#'         server host name.
#'
#'         \item{server_path} character(1) Full deployment path on
#'         server.
#'
#'         \item{username} character(1) User name of SOUNDBoard
#'         deployment account. The user of the account performing the
#'         deployment must have passwordless login credentials to
#'         \code{username@server_host:server_path}.
#'
#'     }
#'
#' @param option character(1) Name of option to retrieve.
#'
#' @return \code{SOUNDBoardOption()} returns NULL, invisibly.
#' 
#' @export
SOUNDBoardOption <-
    function(...)
{
    args <- list(...)
    .stopifnot_SOUNDBoardOption(names(args))

    for (nm in names(args))
        .SOUNDBoardOption[[nm]] <- args[[nm]]
}

#' @rdname SOUNDBoardOption-class
#' 
#' @return \code{getSOUNDBoardOption()} returns the character(1) value
#'     of the requested option.
#' 
#' @export
getSOUNDBoardOption <-
    function(option)
{
    .stopifnot_scalar_character(option)
    .stopifnot_SOUNDBoardOption(option)
    .SOUNDBoardOption[[option]]
}
