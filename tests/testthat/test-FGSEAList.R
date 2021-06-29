context("FGSEAList")

data(
    deseq,
    package = "DESeqAnalysis",
    envir = environment()
)

test_that("RankedList", {
    x <- FGSEAList(
        object = deseq,
        geneSetFiles = c("h" = file.path("cache", "h.all.v7.0.symbols.gmt"))
    )
    expect_s4_class(x, "FGSEAList")
})
