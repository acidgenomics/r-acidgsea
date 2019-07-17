context("RankedList")

value <- methodFormals(
    f = "RankedList",
    signature = "DESeqAnalysis",
    package = "pfgsea"
) %>%
    .[["value"]] %>%
    eval()

with_parameters_test_that(
    "DESeqAnalysis", {
        object <- RankedList(deseq, value = value)
        expect_s4_class(object, "RankedList")
        expect_identical(
            object = names(object),
            expected = "condition_B_vs_A"
        )
    },
    value = value
)

rm(value)

test_that("FGSEAList", {
    object <- RankedList(gsea)
    expect_s4_class(object, "RankedList")
    expect_identical(
        object = names(object),
        expected = "dmso_r1881_vs_etoh"
    )
})

test_that("matrix", {
    data(mat, package = "acidtest", envir = environment())
    object <- RankedList(mat)
    expect_s4_class(object, "RankedList")
    expect_identical(names(object), colnames(mat))
})
