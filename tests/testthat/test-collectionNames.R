context("collectionNames")

test_that("FGSEAList", {
    expect_identical(collectionNames(gsea), "h")
})

test_that("FGSEAList assignment", {
    collectionNames(gsea) <- "XXX"
    expect_identical(collectionNames(gsea), "XXX")
})
