context("convertToHuman")

## Skip on older versions of Bioconductor, due to GRanges subsetting issue.
skip_if_not(BiocManager::version() >= "3.9")

test_that("Already human", {
    object <- convertToHuman(deseq)
    expect_identical(object, deseq)
})

test_that("Mouse", {
    mm <- readRDS(file.path("cache", "mm_deseq.rds"))
    hs <- convertToHuman(mm)
    expect_s4_class(hs, "DESeqAnalysis")
})
