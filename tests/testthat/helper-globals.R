## nolint start
`rowData<-` <- SummarizedExperiment::`rowData<-`
data <- utils::data
isAnExistingUrl <- goalie::isAnExistingUrl
methodFormals <- AcidBase::methodFormals
realpath <- AcidBase::realpath
rowData <- SummarizedExperiment::rowData
tempdir2 <- AcidBase::tempdir2
unlink2 <- AcidBase::unlink2
## nolint end

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
