context("plotEnrichedGeneSets")

test_that("FGSEAList", {
    output <- capture.output(
        plotEnrichedGeneSets(fgsea, collection = "h", n = 1L)
    )
    expect_identical(
        object = output,
        expected = c(
            "",
            "",
            "### condition_B_vs_A {.tabset}",
            "",
            "",
            "",
            "### treatment_D_vs_C {.tabset}",
            ""
        )
    )
})
