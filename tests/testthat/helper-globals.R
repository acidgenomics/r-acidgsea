data(
    deseq,
    package = "DESeqAnalysis",
    envir = environment()
)
data(
    matrix_lfc,
    package = "AcidTest",
    envir = environment()
)
data(
    fgsea,
    package = "AcidGSEA",
    envir = environment()
)

## nolint start
hasInternet <- goalie::hasInternet
methodFormals <- AcidBase::methodFormals
realpath <- AcidBase::realpath
rowData <- SummarizedExperiment::rowData
`rowData<-` <- SummarizedExperiment::`rowData<-`
## nolint end
