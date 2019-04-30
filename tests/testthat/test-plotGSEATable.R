context("plotGSEATable")

test_that("FGSEAList", {
    output <- capture.output(
        plotGSEATable(gsea, collection = "h", n = 2L)
    )
    expect_identical(
        output,
        c(
            "",
            "",
            "### dmso_r1881_vs_etoh",
            ""
        )
    )
})
