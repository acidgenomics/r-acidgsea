data(
    deseq,
    package = "DESeqAnalysis",
    envir = environment()
)
data(
    matrix_lfc,
    package = "acidtest",
    envir = environment()
)
data(
    gsea,
    package = "pfgsea",
    envir = environment()
)

hasInternet <- goalie::hasInternet
skip_on_docker <- goalie::skip_on_docker
