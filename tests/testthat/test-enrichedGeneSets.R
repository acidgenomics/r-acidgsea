context("enrichedGeneSets")

test_that("FGSEAList", {
    object <- enrichedGeneSets(gsea, collection = "h")
    expect_type(object, "list")
    expect_identical(
        names(object),
        c("dmso_r1881_vs_etoh_down", "dmso_r1881_vs_etoh_up")
    )
})
