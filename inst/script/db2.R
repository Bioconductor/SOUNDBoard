library(SOUNDBoard)
library(S4Vectors)

##
## Create / update template
## - SQL tables & queries
##

## # Read file from AWS
downloadAWS <- function(filename) {
    system(
        paste(paste0("aws s3 cp s3://experimenthub/curatedTCGAData/",
              filename),
              "inst/extdata/"))
}

filenames <- c("GBM_GISTIC_AllByGene-20160128.rda",
    "GBM_GISTIC_ThresholdedByGene-20160128.rda",
    "GBM_colData-20160128.rda",
    "GBM_Mutation-20160128.rda")

invisible(lapply(filenames, downloadAWS))

unname(vapply(filenames, function(file) {
    load(paste0("inst/extdata/", file), envir = .GlobalEnv)
}, character(1L)))

GBM_Mutation <- `GBM_Mutation-20160128`[,
    grepl("^TCGA-32-2615", colnames(`GBM_Mutation-20160128`))]
genes <- assay(GBM_Mutation, "Hugo_Symbol")
matchingGenes <- intersect(genes[!is.na(genes)],
    rownames(`GBM_GISTIC_AllByGene-20160128`))
GBM_Mutation <- GBM_Mutation[as.character(assay(GBM_Mutation, "Hugo_Symbol"))
    %in% matchingGenes, ]
GBM_Mutation <- DataFrame(gene = matchingGenes,
    mutation = as.character(assay(GBM_Mutation, "Variant_Classification")))

GBM_GISTIC2 <- `GBM_GISTIC_ThresholdedByGene-20160128`[
    rownames(`GBM_GISTIC_ThresholdedByGene-20160128`) %in% matchingGenes,
        "TCGA-32-2615-01A-01D-0911-01"]

GBM_GISTIC <- `GBM_GISTIC_AllByGene-20160128`[matchingGenes,
    "TCGA-32-2615-01A-01D-0911-01"]
GBM_GISTIC <- GBM_GISTIC[order(rownames(GBM_GISTIC)), ]
GBM_GISTIC <- DataFrame(gene = rownames(GBM_GISTIC),
    Gscore = unname(assay(GBM_GISTIC)[, 1L]),
    GThresh = unname(assay(GBM_GISTIC2[order(rownames(GBM_GISTIC2)), ])[, 1L]))

GBM_colData <-
    `GBM_colData-20160128`[rownames(`GBM_colData-20160128`) %in% "TCGA-32-2615",
        c("Age..years.at.diagnosis.", "gender", "tumor_tissue_site", "race")]
names(GBM_colData)[1:3] <- c("age", "sex", "primary_site")
GBM_colData <- c(DataFrame(case_uid = rownames(GBM_colData)), GBM_colData,
                 DataFrame(diagnosis = "GBM"))


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
    case_uid = "TCGA-32-2615",
    assay = "GISTIC2",
    description = "GISTIC score by gene",
    resource = manage(x, GBM_GISTIC)
)

tbl(x, "assay") <- list(
    case_uid = "TCGA-32-2615",
    assay = "Mutation",
    description = "Mutations by gene",
    resource = manage(x, GBM_Mutation)
)

##
## Prepare report (embed in markdown)
##

tbl(x, "board") %>% filter(board_uid == "GBMset") %>% sbreport()

tbl(x, "cases") %>% filter(case_uid == "TCGA-32-2615") %>%
    select(-board_uid) %>% sbreport()

tbl(x, "assay") %>%
    filter(case_uid == "TCGA-32-2615", assay == "GISTIC2") %>%
    sbreport()

tbl(x, "assay") %>%
    filter(case_uid == "TCGA-32-2615", assay == "Mutation") %>%
    sbreport()

##
## Deploy to server
##

manager <- SOUNDManager(
    x,
    host = "localhost", port = "3838", path = "SOUNDBoard",
    user = "soundboard"
)

deploy(manager)
