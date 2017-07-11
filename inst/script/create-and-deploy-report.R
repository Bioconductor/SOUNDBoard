suppressPackageStartupMessages({
    library(SOUNDBoard)
    library(shiny)
})

x <- SOUNDManager(tempfile("SOUNDBoard-test-"))

tbl(x, "board") <- list(
    board_uid = "Mitochondrial Diseases",
    description = "Clinical management of rare genetic disorders"
)

tbl(x, "cases") <- list(
    board_uid = "Mitochondrial Diseases",
    case_uid = "PATIENT_1",
    age = 36,
    sex = "Male"
)

tbl(x, "assay") <- list(
    case_uid = "PATIENT_1",
    assay = "RNA candidates",
    description = "RNA candidate genes derived from analysis ...",
    resource = manage(x, data.frame())
)

tbl(x, "assay") <- list(
    case_uid = "PATIENT_1",
    assay = "data.frame",
    description = "Demonstrating data frame management",
    resource = manage(x, mtcars)
)

tbl(x, "assay") <- list(
    case_uid = "PATIENT_1",
    assay = "ggplot",
    description = "Demonstrating ggplot management",
    resource = manage(x, ggplot2::qplot(hp, mpg, data=mtcars))
)

myapp <- ShinyAppWidget(
    shinyApp(
        ui = bootstrapPage(
            numericInput('n', 'Number of obs', 100),
            plotOutput('plot')
        ),
        server = function(input, output) {
            output$plot <- renderPlot({
                hist(runif(input$n))
            })
        }
    )
)

tbl(x, "assay") <- list(
    case_uid = "PATIENT_1",
    assay = "shiny",
    description = "Demonstrating shiny application",
    resource = manage(x, myapp)
)

deploy(x, dryrun=FALSE)

x
