context("plotEnrichedUpset")

test_that("FGSEAList", {
    object <- plotEnrichedUpset(
        object = fgsea,
        collection = "h",
        alpha = 0.7
    )
    expect_s3_class(object, "upset")
})
