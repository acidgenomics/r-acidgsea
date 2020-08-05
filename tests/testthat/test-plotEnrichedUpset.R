context("plotEnrichedUpset")

test_that("FGSEAList", {
    object <- fgsea
    alphaThreshold(object) <- 0.9
    object <- plotEnrichedUpset(
        object = object,
        collection = "h"
    )
    expect_s3_class(object, "upset")
})
