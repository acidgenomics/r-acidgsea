context("FGSEAList")

skip_if_not(dir.exists(file.path("~", "msigdb")))

rankedList <- metadata(fgsea)[["rankedList"]]
geneSetFiles <- metadata(fgsea)[["geneSetFiles"]]

test_that("RankedList", {
    x <- FGSEAList(object = rankedList, geneSetFiles = geneSetFiles)
    expect_s4_class(x, "FGSEAList")
})
