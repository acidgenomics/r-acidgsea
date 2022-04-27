#' @name contrastNames
#' @inherit AcidGenerics::contrastNames
#' @note Updated 2020-03-18.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(fgsea)
#'
#' ## FGSEAList ====
#' contrastNames(fgsea)
NULL



## Updated 2019-07-24.
`contrastNames,FGSEAList` <- # nolint
    function(object) {
        names(object[[1L]])
    }



## Updated 2019-07-24.
`contrastNames<-,FGSEAList,character` <- # nolint
    function(object, value) {
        ## Loop across the pathway list and update the contrast names.
        listData <- slot(object, "listData")
        assert(
            isCharacter(value),
            areSameLength(names(listData[[1L]]), value)
        )
        listData <- lapply(X = listData, FUN = `names<-`, value = value)
        slot(object, "listData") <- listData
        ## Ensure the ranked list names stored in `metadata()` are updated.
        names(metadata(object)[["rankedList"]]) <- value
        validObject(object)
        object
    }



#' @rdname contrastNames
#' @export
setMethod(
    f = "contrastNames",
    signature = signature(object = "FGSEAList"),
    definition = `contrastNames,FGSEAList`
)

#' @rdname contrastNames
#' @export
setMethod(
    f = "contrastNames<-",
    signature = signature(
        object = "FGSEAList",
        value = "character"
    ),
    definition = `contrastNames<-,FGSEAList,character`
)
