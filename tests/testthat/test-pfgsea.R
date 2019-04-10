context("pfgsea")

rankedList <- metadata(gsea)[["rankedList"]]
gmtFiles <- metadata(gsea)[["gmtFiles"]]

test_that("pfgsea", {
    x <- pfgsea(rankedList = rankedList, gmtFiles = gmtFiles)
    expect_s4_class(x, "FGSEAList")
})
