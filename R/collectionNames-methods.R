#' @name collectionNames
#' @inherit AcidGenerics::collectionNames
#' @note Updated 2020-09-17.
#' @param ... Additional arguments.
#' @examples
#' data(fgsea)
#'
#' ## FGSEAList ====
#' collectionNames(fgsea)
NULL



## Updated 2019-07-24.
`collectionNames,FGSEAList` <-  # nolint
    function(object) {
        names(object)
    }



## Updated 2020-09-17.
`collectionNames<-,FGSEAList,character` <-  # nolint
    function(object, value) {
        assert(
            isCharacter(value),
            areSameLength(names(object), value)
        )
        names(object) <- value
        names(metadata(object)[["collections"]]) <- value
        names(metadata(object)[["geneSetFiles"]]) <- value
        validObject(object)
        object
    }



#' @rdname collectionNames
#' @export
setMethod(
    f = "collectionNames",
    signature = signature("FGSEAList"),
    definition = `collectionNames,FGSEAList`
)

#' @rdname collectionNames
#' @export
setMethod(
    f = "collectionNames<-",
    signature = signature(
        object = "FGSEAList",
        value = "character"
    ),
    definition = `collectionNames<-,FGSEAList,character`
)
