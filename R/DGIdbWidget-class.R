#' @exportClass DGIdbWidget
#'
#' @export
DGIdbWidget <- SOUNDWidget(
    widget = "DGIdbWidget",
    save = function(x, file) saveRDS(x, file),
    load = function(x, file) readRDS(file),
    report = function(x) sbresource(x),
    where = topenv()
)
