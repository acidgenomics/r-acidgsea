context("geneSet")

test_that("FGSEAList", {
    object <- geneSet(
        object = fgsea,
        collection = 1L,
        set = 1L
    )
    expect_is(object, "character")
})
