context("show")

test_that("FGSEAList", {
    output <- capture.output(show(gsea))
    expect_match(head(output, n = 1L), "FGSEAList")
})
