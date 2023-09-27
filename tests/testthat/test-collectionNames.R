test_that("FgseaList", {
    expect_identical(
        object = collectionNames(fgsea),
        expected = "h_all_v7_0_symbols"
    )
})

test_that("FgseaList assignment", {
    collectionNames(fgsea) <- "XXX"
    expect_identical(collectionNames(fgsea), "XXX")
})
