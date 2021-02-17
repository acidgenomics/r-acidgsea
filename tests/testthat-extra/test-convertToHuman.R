context("convertToHuman")

skip_if_not(hasInternet(url = "https://ensembl.org/"))

test_that("Human (already)", {
    object <- convertToHuman(deseq)
    expect_identical(object, deseq)
})

test_that("Mouse", {
    mm <- readRDS(file.path("cache", "mm_deseq.rds"))
    expect_s4_class(mm, "DESeqAnalysis")
    expect_identical(
        object = dim(mm@data),
        expected = c(500L, 12L)
    )
    hs <- convertToHuman(mm)
    expect_s4_class(hs, "DESeqAnalysis")
    expect_identical(
        object = dim(hs@data),
        expected = c(484L, 12L)
    )
})
