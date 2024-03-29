test_that("FgseaList", {
    object <- fgsea
    alphaThreshold(object) <- 0.99
    collection <- collectionNames(object)[[1L]]
    output <- capture.output({
        markdownTables(
            object = object,
            collection = collection
        )
    })
    output <- toString(output)
    expect_match(
        object = output,
        regexp = "Upregulated"
    )
    expect_match(
        object = output,
        regexp = "Downregulated"
    )
})
