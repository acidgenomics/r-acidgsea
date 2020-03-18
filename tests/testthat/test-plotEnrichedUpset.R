context("plotEnrichedUpset")

test_that("FGSEAList", {
    object <- plotEnrichedUpset(fgsea, collection = "h")
    expect_s3_class(object, "upset")
})
