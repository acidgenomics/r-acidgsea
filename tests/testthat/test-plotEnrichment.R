context("plotEnrichment")

test_that("FGSEAList", {
    output <- capture.output(
        plotEnrichment(gsea, geneSet = "h", n = 1L)
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
