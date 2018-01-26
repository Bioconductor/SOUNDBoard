library(SOUNDBoard)
library(S4Vectors)
library(qtlcharts)
library(curatedTCGAData)
data(geneExpr)
##
## Create / update template
## - SQL tables & queries
##

## # Read file from AWS
gbm <- curatedTCGAData("GBM",
    c("GISTICT", "GISTICA", "Mutation", "RNASeq2GeneNorm"), FALSE)
rownames(gbm[[3]]) <- RaggedExperiment::mcols(gbm[[3]])[["Hugo_Symbol"]]
genesOfInterest <- c("FAF1", "ASTN1", "PROX1", "PARP1", "AKR1C4", "TAF3")
gbm <- gbm[genesOfInterest, rownames(colData(gbm)) == "TCGA-32-2615", ]

GBM_colData <- colData(gbm)
GBM_colData <- GBM_colData[, c("Age..years.at.diagnosis.", "gender",
    "tumor_tissue_site", "race")]
names(GBM_colData)[1:3] <- c("age", "sex", "primary_site")
GBM_colData <- c(DataFrame(case_uid = rownames(GBM_colData)), GBM_colData,
                 DataFrame(diagnosis = "GBM"))
GBM_GISTIC <- assay(gbm[[1L]])
GBM_Mutation <- assay(gbm[[3L]], "Variant_Classification")

## qtlcharts
qtlPlot <- iplotCorr(geneExpr$expr, geneExpr$genotype, reorder = TRUE,
    chartOpts = list(cortile="Correlation Matrix", scattile = "Scatterplot"))

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

tbl(x, "board") <- list(
    board_uid = "OVset",
    description = "Ovarian clinical advancements"
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

tbl(x, "assay") <- list(
    case_uid = "TCGA-32-2615",
    assay = "qtl",
    description = "Quantitative Trait Loci",
    resource = manage(x, qtlPlot)
)
##
## Prepare report (embed in markdown)
##

tbl(x, "board") %>% filter(board_uid == "GBMset") %>% sbreport()

tbl(x, "cases") %>% select(-board_uid) %>% sbreport()

tbl(x, "assay") %>%
    filter(case_uid == "TCGA-32-2615", assay == "GISTIC2") %>%
    sbreport()

tbl(x, "assay") %>%
    filter(case_uid == "TCGA-32-2615", assay == "Mutation") %>%
    sbreport()

tbl(x, "assay") %>%
    filter(case_uid == "TCGA-32-2615", assay == "qtl") %>%
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


## rDGIdb example
digit <- rDGIdb::queryDGIdb("BRCA1")

tbl(x, "assay") <- list(
    case_uid = "TCGA-32-2615",
    assay = "rDGI",
    description = "Druggability",
    resource = manage(x, digit)
)

tbl(x, "assay") %>%
    filter(case_uid == "TCGA-32-2615", assay == "rDGI") %>%
    sbreport()
