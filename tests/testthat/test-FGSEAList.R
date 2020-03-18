context("FGSEAList")

rankedList <- metadata(fgsea)[["rankedList"]]
gmtFiles <- metadata(fgsea)[["gmtFiles"]]

test_that("FGSEAList", {
    x <- FGSEAList(rankedList = rankedList, gmtFiles = gmtFiles)
    expect_s4_class(x, "FGSEAList")
})
