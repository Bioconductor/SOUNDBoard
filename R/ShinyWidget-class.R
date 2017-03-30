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
ShinyAppDirWidget <- SOUNDWidget(
    widget = "ShinyAppDirWidget",
    save = function(x, file) {
        if (!dir.exists(file))
            dir.create(file, recursive = TRUE)
        ## create a tar ball of the app at 'file'
        tar(file.path(file, "ShinyAppDirWidget.tar.gz"),
            files = list.files(sbresource(x), full.names = TRUE),
            compression = "gzip")
    },
    load = function(x, file) {
        pkgPath <- file.path(system.file("shiny", package = "SOUNDBoard"))
        shinyOut <- file.path(pkgPath, basename(file))
        if (!dir.exists(shinyOut))
            dir.create(shinyOut, recursive = TRUE)
        ## untar file to shiny directory and return directory path
        untar(file.path(file, "ShinyAppDirWidget.tar.gz"),
              exdir = shinyOut)
        shinyOut ## TODO: Fix Me
    },
    report = function(x) shiny::shinyAppDir(sbresource(x)),
    where = topenv()
)
