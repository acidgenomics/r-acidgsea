context("collectionNames")

test_that("FGSEAList", {
    expect_identical(collectionNames(gsea), "h")
})
