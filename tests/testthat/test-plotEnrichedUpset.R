context("plotEnrichedUpset")

test_that("FGSEAList", {
    object <- plotEnrichedUpset(gsea, collection = "h")
    expect_s3_class(object, "upset")
})
