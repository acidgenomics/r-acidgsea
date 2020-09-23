context("enrichedGeneSets : FGSEAList")

object <- fgsea
alphaThreshold(object) <- 0.7

args <- list(
    object = object,
    collection = "h"
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
    expect_is(object[[1L]], "character")
})
