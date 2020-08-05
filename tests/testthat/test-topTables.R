context("topTables")

test_that("FGSEAList", {
    object <- fgsea
    alphaThreshold(object) <- 0.99
    output <- capture.output({
        topTables(
            object = object,
            collection = "h"
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
