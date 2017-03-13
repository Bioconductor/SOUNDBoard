library(SOUNDBoard)

##
## Create / update template
## - SQL tables & queries
##

##
## Assemble
## 

## x <- SOUNDManager(tempfile())
x <- SOUNDManager(tempfile(), "~/a/SOUNDBoard/inst/template/SOUNDBoard.sql")

tbl(x, "board") <- list(
    board_uid = "MitoNET",
    description = "Clinical management of rare genetic disorders"
)

tbl(x, "cases") <- list(
    board_uid = "MitoNET",
    case_uid = "PATIENT_1",
    age = 36L,
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

##
## Prepare report (embed in markdown)
## 

tbl(x, "board") %>% filter(board_uid == "MitoNET") %>% sbreport()

tbl(x, "cases") %>% filter(case_uid == "PATIENT_1") %>%
    select(-board_uid) %>% sbreport()

tbl(x, "assay") %>%
    filter(case_uid == "PATIENT_1", assay == "data.frame") %>%
    sbreport()

tbl(x, "assay") %>%
    filter(case_uid == "PATIENT_1", assay == "ggplot") %>%
    sbreport()

##
## Deploy to server
##

manager <- SOUNDManager(
    x,
    host = "localhost", port = "3838", path = "SOUNDBoard",
    username = "soundboard"
)

deploy(manager)
