context("combine")

test_that("FGSEAList", {
    x <- gsea
    y <- x
    expect_error(
        object = combine(x, y),
        regexp = "contrastNames"
    )
    contrastNames(x) <- paste0("x_", contrastNames(x))
    contrastNames(y) <- paste0("y_", contrastNames(y))
    c <- combine(x, y)
    expect_s4_class(c, "FGSEAList")
})
