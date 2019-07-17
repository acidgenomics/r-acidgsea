context("enrichedGeneSets")

test_that("FGSEAList", {
    object <- enrichedGeneSets(gsea, collection = "h", flatten = TRUE)
    expect_type(object, "list")
    expect_identical(
        object = names(object),
        expected = c("dmso_r1881_vs_etoh_down", "dmso_r1881_vs_etoh_up")
    )
})

test_that("Disable flatten mode", {
    object <- enrichedGeneSets(gsea, collection = "h", flatten = FALSE)
    expect_type(object, "list")
    expect_identical(
        object = names(object),
        expected = "dmso_r1881_vs_etoh"
    )
})
