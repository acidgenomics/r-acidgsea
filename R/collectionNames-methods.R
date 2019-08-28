#' @name collectionNames
#' @inherit bioverbs::collectionNames
#' @note Updated 2019-08-28.
#' @param ... Additional arguments.
#' @examples
#' data(gsea)
#' collectionNames(gsea)
NULL



#' @rdname collectionNames
#' @name collectionNames
#' @importFrom bioverbs collectionNames
#' @usage collectionNames(object, ...)
#' @export
NULL

#' @rdname collectionNames
#' @name collectionNames<-
#' @importFrom bioverbs collectionNames<-
#' @usage collectionNames(object, ...) <- value
#' @export
NULL



## Updated 2019-07-24.
`collectionNames,FGSEAList` <-  # nolint
    function(object) {
        names(object)
    }



#' @rdname collectionNames
#' @export
setMethod(
    f = "collectionNames",
    signature = signature("FGSEAList"),
    definition = `collectionNames,FGSEAList`
)



## Updated 2019-07-24.
`collectionNames<-,FGSEAList,character` <-  # nolint
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
    definition = `collectionNames<-,FGSEAList,character`
)
