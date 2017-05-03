#' @exportClass ShinyAppWidget
#'
#' @export
ShinyAppWidget <- SOUNDWidget(
    widget = "ShinyAppWidget",
    save = function(x, file) saveRDS(x, file),
    load = function(x, file) readRDS(file),
    report = function(x) sbresource(x),
    where = topenv()
)

#' @exportClass ShinyAppDirWidget
#'
#' @export
#' @importFrom utils tar untar
ShinyAppDirWidget <- SOUNDWidget(
    widget = "ShinyAppDirWidget",
    save = function(x, file) {
        owd <- setwd(sbresource(x))
        on.exit(setwd(owd))
        ## create a tar ball of the app at 'file'
        tar(tarfile = file, compression = "gzip")
    },
    load = function(x, file) {
        ## untar file to shiny directory and return directory path
        out <- tempfile()
        untar(file, exdir = out)
        ShinyAppDirWidget(out)
    },
    report = function(x) shiny::shinyAppDir(sbresource(x)),
    where = topenv()
)
