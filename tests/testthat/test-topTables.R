context("topTables")

test_that("FGSEAList", {
    output <- capture.output({
        topTables(
            object = fgsea,
            collection = "h",
            alpha = 0.99
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
