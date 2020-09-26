context("FGSEAList")

skip_if_not(dir.exists(file.path("~", "msigdb")))

deseq <- metadata(fgsea)[["deseq"]]
geneSetFiles <- metadata(fgsea)[["geneSetFiles"]]

test_that("RankedList", {
    x <- FGSEAList(
        object = deseq,
        geneSetFiles = geneSetFiles
    )
    expect_s4_class(x, "FGSEAList")
})
