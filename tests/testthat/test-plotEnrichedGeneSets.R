context("plotEnrichedGeneSets")

test_that("FGSEAList", {
    output <- capture.output(
        plotEnrichedGeneSets(gsea, collection = "h", n = 1L)
    )
    expect_identical(
        object = output,
        expected = c(
            "",
            "",
            "### dmso_r1881_vs_etoh {.tabset}",
            "",
            "",
            "",
            "#### HALLMARK_E2F_TARGETS",
            "",
            "",
            "",
            "#### HALLMARK_MYOGENESIS",
            ""
        )
    )
})