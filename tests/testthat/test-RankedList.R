context("RankedList")

test_that("DESeqAnalysis", {
    x <- RankedList(deseq)
    expect_s4_class(x, "RankedList")
    expect_identical(
        object = names(x),
        expected = "condition_B_vs_A"
    )
})
