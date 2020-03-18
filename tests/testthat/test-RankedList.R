context("RankedList")

value <- eval(methodFormals(
    f = "RankedList",
    signature = "DESeqAnalysis",
    package = "acidgsea"
)[["value"]])

with_parameters_test_that(
    "DESeqAnalysis", {
        object <- RankedList(deseq, value = value)
        expect_s4_class(object, "RankedList")
        expect_identical(
            object = names(object),
            expected = c("condition_B_vs_A", "treatment_D_vs_C")
        )
    },
    value = value
)

rm(value)

test_that("Average values for duplicate gene symbols", {
    x <- deseq
    rowData <- rowData(x@data)
    ## Rle is subsettable with `[[`.
    ## nolint start
    rowData[["geneName"]][2L] <- rowData[["geneName"]][1L]
    rowData[["geneName"]][4L] <- rowData[["geneName"]][3L]
    ## nolint end
    rowData(x@data) <- rowData
    y <- RankedList(x)
    expect_s4_class(y, "RankedList")
    ## Averaging 'stat' value for 2 gene symbols: DPM1, TSPAN6.
    expect_true(nrow(x@data) - length(y[[1L]]) == 3L)
})

test_that("FGSEAList", {
    object <- RankedList(gsea)
    expect_s4_class(object, "RankedList")
    expect_identical(
        object = names(object),
        expected = "condition_B_vs_A"
    )
})

test_that("matrix", {
    object <- RankedList(matrix_lfc)
    expect_s4_class(object, "RankedList")
    expect_identical(names(object), colnames(matrix_lfc))
})
