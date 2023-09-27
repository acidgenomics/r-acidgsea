test_that("FgseaList", {
    object <- fgsea
    collection <- collectionNames(object)[[1L]]
    set <- geneSetNames(object = object, collection = collection)[[1L]]
    x <- geneSet(
        object = object,
        collection = collection,
        set = set
    )
    expect_type(x, "character")
})
