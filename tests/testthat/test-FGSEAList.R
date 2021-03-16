context("FGSEAList")

skip_if_not(dir.exists(file.path("~", "msigdb")))

data(
    deseq,
    package = "DESeqAnalysis",
    envir = environment()
)

geneSetFiles <- metadata(fgsea)[["geneSetFiles"]]

test_that("RankedList", {
    x <- FGSEAList(
        object = deseq,
        geneSetFiles = geneSetFiles
    )
    expect_s4_class(x, "FGSEAList")
})
