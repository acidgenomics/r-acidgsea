test_that("FGSEAList", {
    object <- fgsea
    collection <- collectionNames(object)[[1L]]
    output <- capture.output({
        plotEnrichedGeneSets(
            object = object,
            collection = collection,
            n = 1L
        )
    })
    expect_identical(
        object = output[[3L]],
        expected = "### condition_B_vs_A {.tabset}"
    )
})
