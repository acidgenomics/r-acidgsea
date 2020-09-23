context("plotEnrichedGeneSets")

skip_if_not(dir.exists(file.path("~", "msigdb")))

test_that("FGSEAList", {
    output <- capture.output({
        plotEnrichedGeneSets(fgsea, collection = "h", n = 1L)
    })
    expect_identical(
        object = output[[3L]],
        expected = "### condition_B_vs_A {.tabset}"
    )
})
