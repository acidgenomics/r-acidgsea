context("enrichedGeneSets")

test_that("FGSEAList", {
    object <- enrichedGeneSets(fgsea, collection = "h", flatten = TRUE)
    expect_type(object, "list")
    expect_identical(
        object = names(object),
        expected = c(
            "condition_B_vs_A_up",
            "condition_B_vs_A_down",
            "treatment_D_vs_C_up",
            "treatment_D_vs_C_down"
        )
    )
})

test_that("Disable flatten mode", {
    object <- enrichedGeneSets(fgsea, collection = "h", flatten = FALSE)
    expect_type(object, "list")
    expect_identical(
        object = names(object),
        expected = c(
            "condition_B_vs_A",
            "treatment_D_vs_C"
        )
    )
})
