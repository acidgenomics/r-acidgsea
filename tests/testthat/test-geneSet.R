context("geneSet")

test_that("FGSEAList", {
    object <- geneSet(
        object = fgsea,
        collection = "h",
        set = "HALLMARK_P53_PATHWAY"
    )
    expect_is(object, "character")
})
