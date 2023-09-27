#' @name collectionNames
#' @inherit AcidGenerics::collectionNames
#' @note Updated 2022-04-27.
#' @param ... Additional arguments.
#' @examples
#' data(fgsea)
#'
#' ## FgseaList ====
#' object <- fgsea
#' collectionNames(object)
NULL



## Updated 2019-07-24.
`collectionNames,FgseaList` <- # nolint
    function(object) {
        names(object)
    }



## Updated 2020-09-17.
`collectionNames<-,FgseaList,character` <- # nolint
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
    signature = signature(object = "FgseaList"),
    definition = `collectionNames,FgseaList`
)

#' @rdname collectionNames
#' @export
setMethod(
    f = "collectionNames<-",
    signature = signature(
        object = "FgseaList",
        value = "character"
    ),
    definition = `collectionNames<-,FgseaList,character`
)
