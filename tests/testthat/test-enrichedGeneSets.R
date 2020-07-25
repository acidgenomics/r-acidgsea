context("enrichedGeneSets : FGSEAList")

args <- list(
    object = fgsea,
    collection = "h",
    alpha = 0.7
)

test_that("Upregulated gene sets", {
    object <- do.call(
        what = enrichedGeneSets,
        args = c(args, direction = "up")
    )
    expect_type(object, "list")
})

test_that("Downregulated gene sets", {
    object <- do.call(
        what = enrichedGeneSets,
        args = c(args, direction = "down")
    )
    expect_type(object, "list")
})

test_that("Enriched in both directions", {
    object <- do.call(
        what = enrichedGeneSets,
        args = c(args, direction = "both")
    )
    expect_type(object, "list")
    expect_identical(
        object = names(object),
        expected = c(
            "condition_B_vs_A",
            "treatment_D_vs_C"
        )
    )
    expect_identical(
        object = length(object[["condition_B_vs_A"]]),
        expected = 9L
    )
    expect_identical(
        object = length(object[["treatment_D_vs_C"]]),
        expected = 21L
    )
})
