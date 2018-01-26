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

#' @exportClass SOUNDBoardWidget
#'
#' @export
.SOUNDBoardWidget <- setClass(
    "SOUNDBoardWidget",
    contains = "ShinyAppWidget",
    slots = c(
        resource = "ShinyAppWidget"
    )
)

#' @rdname SOUNDBoardWidget-class
#'
#' @title Interactive display of SOUND resources through a the
#' ShinyAppWidget class
#'
#' @description This function will display a \link{SOUNDManager-class} object
#' as an interactive shiny widget. The widget allows the user to select
#' by board, case and assay. The displays will react depending on selected
#' components of the database. The database is to be created beforehand.
#'
#' @param soundmgr A \linkS4class{SOUNDManager} class
#'
#' @export
SOUNDBoardWidget <- function(soundmgr) {
    ShinyAppWidget(
        shinyApp(
            ui = fluidPage(
                includeCSS(SOUNDBoard:::.options$get("css")),
                fluidRow(
                    column(8, titlePanel("SOUNDBoard Report")),
                    column(4, br(),
                        img(src = "../inst/resources/html/sound_wordmark.svg",
                        align = "right", height = 72, width = 96,
                        style = "margin-left:10px"))
                ),
                fluidRow(
                    column(3,
                            wellPanel(
                                h3("Select Board"),
                                selectInput("board", "Board",
                                choices = as.data.frame(tbl(soundmgr,
                                    "board"))[["board_uid"]]),
                                helpText(h3("Select Case")),
                                uiOutput("cases"),
                                helpText(h3("Select Assay")),
                                selectInput("assay", "Assay",
                                    choices = unique(as.data.frame(
                                        tbl(soundmgr, "assay"))[["assay"]]))
                            )),
                    column(9,
                            fluidRow(
                                column(12, DT::dataTableOutput("casetable")),
                                column(12, DT::dataTableOutput("assaytable")))
                    )
            )),
            server = function(input, output, session) {
                pt_table <- reactive({
                    re_tb <- as.data.frame(tbl(soundmgr, "cases"))
                    re_tb <- subset(re_tb, board_uid == input$board)
                })
                output$cases <- renderUI({
                    ptdat <- pt_table()
                    selectInput("case", "Case",
                        choices =  unique(as.character(ptdat[["case_uid"]])))
                })
                output$casetable <- DT::renderDataTable({
                    ptdat <- pt_table()
                    DT::datatable(ptdat,
                        rownames = FALSE, selection = 'none',
                        callback = DT::JS("table.on('click.dt', 'td', function() {
                        var rowIndx = table.cell(this).index().row;
                        rowIndx += 1;
                        Shiny.onInputChange('rows', rowIndx);
                        });"),
                    options = list(dom = "ftpi")
                    )
                })
                case_uid <- eventReactive(input$rows, {
                    pttab <- pt_table()
                    pttab[input$rows, "case_uid"]
                })
                output$assaytable <- DT::renderDataTable({
                    cid <- case_uid()
                    tbl(soundmgr, "assay") %>%
                        filter(assay == input$assay, case_uid == cid) %>%
                        sbreport()
                })
            },
            options = list(
                width = "100%", height = 800
            )
        )
    )
}
