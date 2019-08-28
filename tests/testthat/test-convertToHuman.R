context("convertToHuman")

test_that("Already human", {
    object <- convertToHuman(deseq)
    expect_identical(object, deseq)
})

test_that("Mouse", {
    mm <- readRDS(file.path("cache", "mm_deseq.rds"))
    hs <- convertToHuman(mm)
    expect_s4_class(hs, "DESeqAnalysis")
})
