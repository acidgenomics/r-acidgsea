#' Gene set collection names
#'
#' @name collectionNames
#' @inheritParams params
#'
#' @examples
#' data(gsea)
#' collectionNames(gsea)
NULL



collectionNames.FGSEAList <-  # nolint
    function(object) {
        names(object)
    }



#' @rdname collectionNames
#' @export
setMethod(
    f = "collectionNames",
    signature = signature("FGSEAList"),
    definition = collectionNames.FGSEAList
)



`collectionNames<-.FGSEAList,character` <-  # nolint
    function(object, value) {
        assert(
            isCharacter(value),
            areSameLength(names(object), value)
        )
        names(object) <- value
        names(metadata(object)[["gmtFiles"]]) <- value
        validObject(object)
        object
    }



#' @rdname collectionNames
#' @export
setMethod(
    f = "collectionNames<-",
    signature = signature(
        object = "FGSEAList",
        value = "character"
    ),
    definition = `collectionNames<-.FGSEAList,character`
)
