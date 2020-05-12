context("FGSEAList")

skip_if_not(dir.exists(file.path("~", "msigdb")))

rankedList <- metadata(fgsea)[["rankedList"]]
gmtFiles <- metadata(fgsea)[["gmtFiles"]]

test_that("FGSEAList", {
    x <- FGSEAList(rankedList = rankedList, gmtFiles = gmtFiles)
    expect_s4_class(x, "FGSEAList")
})
