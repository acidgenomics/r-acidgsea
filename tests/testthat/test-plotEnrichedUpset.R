context("plotEnrichedUpset")

test_that("FGSEAList", {
    object <- fgsea
    alphaThreshold(object) <- 0.9
    collection <- collectionNames(object)[[1L]]
    object <- plotEnrichedUpset(
        object = object,
        collection = collection
    )
    expect_s3_class(object, "ggplot")
})
