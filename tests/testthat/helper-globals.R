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
methodFormals <- basejump::methodFormals
realpath <- basejump::realpath
rowData <- basejump::rowData
`rowData<-` <- basejump::`rowData<-`
skip_on_docker <- goalie::skip_on_docker
## nolint end
