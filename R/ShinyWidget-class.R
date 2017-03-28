.ShinyAppWidget <- setClass("ShinyAppWidget", contains = "SOUNDWidget")
#' @exportClass ShinyAppWidget
#'
#' @export
ShinyAppWidget <- SOUNDWidget(
    widget = "ShinyAppWidget",
    save = function(x, file) saveRDS(x, file),
    load = function(x, file) readRDS(file),
    report = function(x) sbresource(x)
)

.ShinyAppDirWidget <- setClass("ShinyAppDirWidget", contains = "SOUNDWidget")
#' @exportClass ShinyAppDirWidget
#'
#' @export
ShinyAppDirWidget <- SOUNDWidget(
    widget = "ShinyAppDirWidget",
    save = function(x, file) {
        ## create a tar ball of the app at 'file'
        tar(file.path(file, "ShinyAppDirWidget.tar"), files = dir(file))
    },
    load = function(x, file) {
        ## untar file to temporary directory (where?) and return directory path
        shinyOut <- system.file("shiny", basename(file), package = "SOUNDBoard")
        untar(file.path(file, "ShinyAppDirWidget.tar"),
              exdir = shinyOut)
        shinyOut
    },
    report = function(x) sbresource(x)         # FIXME: ???
)
