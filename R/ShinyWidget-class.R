#' @exportClass ShinyAppWidget
#'
#' @export
ShinyAppWidget <- SOUNDWidget(
    "ShinyAppWidget",
    save = function(x, file) saveRDS(x, file),
    load = function(x, file) readRDS(file),
    report = function(x) sbresource(x)
)

#' @exportClass ShinyAppDirWidget
#'
#' @export
ShinyAppDirWidget <- SOUNDWidget(
    "ShinyAppDirWidget",
    save = function(x, file) {
        ## create a tar ball of the app at 'file'
    },
    load = function(x, file) {
        ## untar file to temporary directory (where?) and return directory path
    },
    report = function(x) sbresource(x)         # FIXME: ???
)
