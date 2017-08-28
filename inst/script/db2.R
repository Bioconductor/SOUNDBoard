library(SOUNDBoard)
##
## Create / update template
## - SQL tables & queries
##

## # Read file from AWS
system(paste("aws s3 cp",
"s3://experimenthub/curatedTCGAData/GBM_GISTIC_AllByGene-20160128.rda",
"inst/extdata/"))
system(paste("aws s3 cp",
"s3://experimenthub/curatedTCGAData/GBM_colData-20160128.rda",
"inst/extdata/"))
system(paste("aws s3 cp",
"s3://experimenthub/curatedTCGAData/GBM_Mutation-20160128.rda",
"inst/extdata/"))

load("inst/extdata/GBM_GISTIC_AllByGene-20160128.rda")
genes <- rownames(`GBM_GISTIC_AllByGene-20160128`)[
    !grepl("ENSG", rownames(`GBM_GISTIC_AllByGene-20160128`))]
GBM_GISTIC <- `GBM_GISTIC_AllByGene-20160128`[genes[1:10],
    "TCGA-32-2615-01A-01D-0911-01"]
GBM_GISTIC <- DataFrame(assay(GBM_GISTIC))
GBM_GISTIC <- DataFrame(gene = rownames(GBM_GISTIC),
                        Gscore = GBM_GISTIC[[1L]])

load("inst/extdata/GBM_colData-20160128.rda")
GBM_colData <-
    `GBM_colData-20160128`[rownames(`GBM_colData-20160128`) %in% "TCGA-32-2615",
        c("Age..years.at.diagnosis.", "gender", "tumor_tissue_site")]
GBM_colData <- c(DataFrame(case_uid = rownames(GBM_colData)), GBM_colData,
                 DataFrame(diagnosis = "GBM"))
names(GBM_colData)[2:4] <- c("age", "sex", "primary_site")

load("inst/extdata/GBM_Mutation-20160128.rda")
GBM_Mutation <- `GBM_Mutation-20160128`[,
    grepl("^TCGA-32-2615", colnames(`GBM_Mutation-20160128`))]

##
## Assemble
##

## x <- SOUNDManager(tempfile())
x <- SOUNDManager(tempfile(), "inst/template/KBoard.sql",
                  "inst/template/SOUNDClinical.Rmd")

tbl(x, "board") <- list(
    board_uid = "GBMset",
    description = "Clinical management of GlioBlastoma Multiforme"
)

tbl(x, "cases") <- c(list(
    board_uid = "GBMset"),
    as.list(GBM_colData)
)

tbl(x, "assay") <- list(
    case_uid = GBM_colData[["case_uid"]],
    assay = "GISTIC2",
    description = "GISTIC score by gene",
    resource = manage(x, GBM_GISTIC)
)

## TODO: FIX Mutation dataset
tbl(x, "assay") <- list(
    case_uid = GBM_colData[["case_uid"]],
    assay = "mutation",
    description = "Mutations by gene",
    resource = manage(x, GBM_Mutation)
)

##
## Prepare report (embed in markdown)
##

tbl(x, "board") %>% filter(board_uid == "GBMset") %>% sbreport()

tbl(x, "cases") %>% filter(case_uid == GBM_colData[["case_uid"]]) %>%
    select(-board_uid) %>% sbreport()

tbl(x, "assay") %>%
    filter(case_uid == GBM_colData[["case_uid"]], assay == "GISTIC2") %>%
    sbreport()

tbl(x, "assay") %>%
    filter(case_uid == GBM_colData[["case_uid"]], assay == "Mutation") %>%
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
