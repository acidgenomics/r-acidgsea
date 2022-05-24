test_that("FGSEAList", {
    object <- fgsea
    output <- capture.output(show(object))
    expect_match(head(output, n = 1L), "FGSEAList")
})
