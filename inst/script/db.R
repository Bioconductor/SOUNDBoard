library(SOUNDBoard)
library(dplyr)

##
## Create / update template
## - SQL tables & queries
##

##
## Assemble
## 

x <- DBWidget(tempfile())

tbl(x, "board") <- list(
    board_uid = "MitoNET",
    description = "Clinical management of rare genetic disorders"
)

tbl(x, "cases") <- list(
    board_uid = "MitoNET",
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
    resource = manage(x, qplot(hp, mpg, data=mtcars))
)

##
## Prepare report (embed in markdown)
## 

tbl(x, "board") %>% filter(board_uid == "MitoNET") %>% report()

tbl(x, "cases") %>% filter(case_uid == "PATIENT_1") %>%
    select(-board_uid) %>% report()

tbl(x, "assay") %>%
    filter(case_uid == "PATIENT_1", assay == "data.frame") %>%
    report()

tbl(x, "assay") %>%
    filter(case_uid == "PATIENT_1", assay == "ggplot") %>%
    report()

##
## Deploy to server
##

manager <- SOUNDManager(
    x,
    host = "localhost", port = "3838", path = "SOUNDBoard",
    username = "soundboard"
)

deploy(manager)
