context("FGSEAList")

rankedList <- metadata(gsea)[["rankedList"]]
gmtFiles <- metadata(gsea)[["gmtFiles"]]

test_that("FGSEAList", {
    x <- FGSEAList(rankedList = rankedList, gmtFiles = gmtFiles)
    expect_s4_class(x, "FGSEAList")
})
