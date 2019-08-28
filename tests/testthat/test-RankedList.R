context("RankedList")

value <- eval(methodFormals(
    f = "RankedList",
    signature = "DESeqAnalysis",
    package = "pfgsea"
)[["value"]])

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

test_that("Average values for duplicate gene symbols", {
    x <- deseq
    rowData <- rowData(x@data)
    rowData[["geneName"]][2L] <- rowData[["geneName"]][1L]
    rowData[["geneName"]][4L] <- rowData[["geneName"]][3L]
    rowData(x@data) <- rowData
    y <- RankedList(x)
    expect_s4_class(y, "RankedList")
    expect_true(nrow(x@data) - length(y[[1]]) == 2L)
})

test_that("FGSEAList", {
    object <- RankedList(gsea)
    expect_s4_class(object, "RankedList")
    expect_identical(
        object = names(object),
        expected = "dmso_r1881_vs_etoh"
    )
})

test_that("matrix", {
    object <- RankedList(matrix_lfc)
    expect_s4_class(object, "RankedList")
    expect_identical(names(object), colnames(matrix_lfc))
})
