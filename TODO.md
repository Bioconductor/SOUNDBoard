- [ ] add a simple graphical (ggplot2 offline version of) plotly-based
  widget

- [ ] R/ start a simple class hierarchy -- strictly _R_ data types


    ```
    .SOUNDBoardWidget <- setClass("SOUNDBoardWidget")

    .SOUNDBoardTable <- setClass(
        "SOUNDBoardTable",
        contains = "SOUNDBoardWidget",
        slots = c(data_source = "data.frame")
    )

    ## import(dplyr)

    setOldClass(c("sqlite_src", "sql_src"))

    .SOUNDBoardSQL <- setClass(
        "SOUNDBoardTable",
        contains = "SOUNDBoardWidget",
        slots = c(data_source = "sql_src")
    )

    .SOUNDBoardPlot <- setClass(
        "SOUNDBoardPlot",
        contains = "SOUNDBoardWidget"
    )
    ```
    
- [ ] R generic / method to render widgets in markdown reportx


    ```
    setGeneric("renderWidget",
        function(x, ...) standardGeneric("renderWidget")
    )
    
    setMethod("renderWidget", "SOUNDBoardTable",
        function(x, ...)
    {
        ## in markdown, `renderWidget(sbtable)` produces necessary output.
        ## get data, data = data_source(sbtable) %>% select(...)??
        ## render DT(data)
    })
    ```
        
- [ ] vignette updated to use these conventions.
