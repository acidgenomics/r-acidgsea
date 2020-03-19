context("collectionNames")

test_that("FGSEAList", {
    expect_identical(collectionNames(fgsea), "h")
})

test_that("FGSEAList assignment", {
    collectionNames(fgsea) <- "XXX"
    expect_identical(collectionNames(fgsea), "XXX")
})
