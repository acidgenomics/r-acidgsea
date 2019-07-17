context("combine")

test_that("FGSEAList", {
    x <- gsea
    y <- x
    expect_error(
        object = combine(x, y),
        regexp = "contrastNames"
    )
    contrastNames(x) <- "x"
    contrastNames(y) <- "y"
    c <- combine(x, y)
    expect_s4_class(c, "FGSEAList")
})
